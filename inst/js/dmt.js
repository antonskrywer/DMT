

window.resetDMT = function () {
  console.log("🔄 Resetting DMT state");

  // ----------------------------
  // Tone.js reset
  // ----------------------------
  try {
    Tone.Transport.stop();
    Tone.Transport.cancel(0);
    Tone.Transport.position = 0;
  } catch (e) {
    console.warn("Transport reset failed", e);
  }

  // ----------------------------
  // Remove scheduled parts
  // ----------------------------
  if (window.part) {
    try {
      window.part.dispose();
    } catch (e) {}
    window.part = null;
  }

  // ----------------------------
  // Reset UI grid
  // ----------------------------
  document.querySelectorAll(".cell").forEach(cell => {
    cell.classList.remove("active");
  });

};

window.initDMT = function () {

  // Tone.js kann (v.a. auf der ersten Seite) noch nachladen
  if (!window.Tone) {
    setTimeout(window.initDMT, 100);
    return;
  }

  // Instruktions-Seiten: { lockGrid, highlightRow, demo } (siehe intro.R)
  const cfg = window.dmtIntroConfig || {};

  // --------------------------------------------------
  // 🎯 READ TEMPO FROM SHINY
  // --------------------------------------------------

  let tempo = 100;

  if (window.Shiny && Shiny.shinyapp) {
    const inputs = Shiny.shinyapp.$inputValues;

    if (inputs && inputs.tempo_init !== undefined && !isNaN(inputs.tempo_init)) {
      tempo = Number(inputs.tempo_init);
    }
  }

  console.log("DMT init — tempo:", tempo);

  // --------------------------------------------------
  // ▶️ START AUDIO + APPLY TEMPO (SAFE)
  // --------------------------------------------------

  Tone.start().then(() => {
    Tone.Transport.bpm.value = tempo;
  });

  // --------------------------------------------------
  // STATE
  // --------------------------------------------------

  const rows = 3;
  const cols = 16;

  let matrix = Array.from({ length: rows }, () => Array(cols).fill(0));
  let cells = [];

  let step = 0;
  let sequencerRunning = false;
  let stimulusRunning = false;

  // --------------------------------------------------
  // GRID
  // --------------------------------------------------

  function buildGrid() {

    for (let r = 0; r < rows; r++) {

      cells[r] = [];

      const rowDiv = document.getElementById("row" + r);
      if (!rowDiv) continue;

      rowDiv.innerHTML = "";

      for (let c = 0; c < cols; c++) {

        const cell = document.createElement("div");
        cell.className = "cell";

        cell.onclick = function () {

          if (stimulusRunning || cfg.lockGrid) return;

          matrix[r][c] = matrix[r][c] ? 0 : 1;
          cell.classList.toggle("active");

          if (window.Shiny) {
            Shiny.setInputValue("sequencer_state", matrix, { priority: "event" });
          }
        };

        rowDiv.appendChild(cell);
        cells[r][c] = cell;
      }
    }
  }

  buildGrid();

  if (cfg.lockGrid) {
    const wrapper = document.getElementById("sequencer-wrapper");
    if (wrapper) wrapper.classList.add("locked");
  }

  if (cfg.highlightRow !== undefined && cfg.highlightRow !== null) {
    const labels = document.querySelectorAll(".sequencer .inst");
    for (let r = 0; r < rows; r++) {
      const cls = (r === cfg.highlightRow) ? "highlighted" : "dimmed";
      if (labels[r]) labels[r].classList.add(cls);
      const rowDiv = document.getElementById("row" + r);
      if (rowDiv) rowDiv.classList.add(cls);
    }
  }


  function loadSequencer(state) {

    if (!state) return;

    matrix = state;

    for (let r = 0; r < rows; r++) {
      for (let c = 0; c < cols; c++) {

        if (matrix[r][c]) {
          cells[r][c].classList.add("active");
        } else {
          cells[r][c].classList.remove("active");
        }

      }
    }

    if (window.Shiny) {
      Shiny.setInputValue("sequencer_state", matrix, {priority: "event"});
    }
  }

  // --------------------------------------------------
  // 🔁 HARD RESET (CLEAN + SAFE ORDER)
  // --------------------------------------------------
  resetDMT();

  console.log("initialSequencerState", window.initialSequencerState);

  if (window.initialSequencerState !== null &&
      window.initialSequencerState !== undefined) {
    loadSequencer(window.initialSequencerState);
  } else {
    // BUGFIX: ohne diesen Reset behaelt Shiny serverseitig den
    // sequencer_state-Wert der VORHERIGEN Seite, solange der Nutzer auf
    // der neuen (visuell leeren) Seite keine einzige Zelle anklickt.
    // Klickt er dann direkt auf Next, wertet dmt_get_answer() faelschlich
    // das alte Pattern der letzten Seite als Antwort - z.B. ein zuvor
    // korrekt geloestes Practice-Pattern, was zu einer faelschlich als
    // "Correct!" gewerteten leeren Eingabe fuehren kann.
    if (window.Shiny) {
      Shiny.setInputValue("sequencer_state", matrix, { priority: "event" });
    }
  }

  // --------------------------------------------------
  // GRID ENABLE / DISABLE
  // --------------------------------------------------

  function setGridEnabled(enabled) {
    cells.flat().forEach(cell => {
      cell.style.pointerEvents = enabled ? "auto" : "none";
      cell.style.opacity = enabled ? 1 : 0.5;
    });
  }

  // --------------------------------------------------
  // SHOW SOLUTION
  // --------------------------------------------------

  if (window.showSolution && window.drumStimulus) {

    setGridEnabled(false);

    for (let r = 0; r < rows; r++) {
      for (let c = 0; c < cols; c++) {
        matrix[r][c] = 0;
        cells[r][c].classList.remove("active");
      }
    }

    const rowMap = {
      "HiHat": 0,
      "Snare": 1,
      "Kick": 2
    };

    window.drumStimulus.forEach(note => {
      const r = rowMap[note.Instrument];
      const c = note.BeatPositionSixteenth - 1;

      if (cells[r] && cells[r][c]) {
        matrix[r][c] = 1;
        cells[r][c].classList.add("active");
      }
    });

    if (window.Shiny) {
      Shiny.setInputValue("sequencer_state", matrix, { priority: "event" });
    }
  }

  // --------------------------------------------------
  // BUTTON STATE
  // --------------------------------------------------

  function setButtonState(mode) {

  const stimBtn = document.getElementById("play_stimulus");
  const seqBtn = document.getElementById("play_sequencer");

    if (mode === "idle") {
      if (stimBtn) stimBtn.disabled = false;
      if (seqBtn) seqBtn.disabled = false;

    } else if (mode === "stimulus") {
      if (stimBtn) stimBtn.disabled = false;
      if (seqBtn) seqBtn.disabled = true;

    } else if (mode === "sequencer") {
      if (stimBtn) stimBtn.disabled = true;
      if (seqBtn) seqBtn.disabled = false;
    }
  }

  // --------------------------------------------------
  // AUDIO
  // --------------------------------------------------

  // Einmal pro Tone-Instanz laden und seitenuebergreifend wiederverwenden
  // (sonst wird bei jedem Seitenwechsel neu geladen/dekodiert und die ersten
  // Schlaege koennen "buffer not loaded" ausloesen).
  if (!window.dmtDrum || window.dmtDrumTone !== Tone) {
    window.dmtDrum = new Tone.Players({
      HiHat: "audio/hihat.wav",
      Snare: "audio/snare.wav",
      Kick: "audio/kick.wav"
    }).toDestination();
    window.dmtDrumTone = Tone;
    window.dmtLastStart = {};
  }
  const drum = window.dmtDrum;

  // Tone wirft "Start time must be strictly greater than previous start
  // time", wenn ein Player mit gleicher/frueherer Startzeit erneut gestartet
  // wird (z.B. bei schnellem Stop/Start innerhalb der Lookahead-Zeit).
  window.dmtLastStart = window.dmtLastStart || {};

  function playSample(inst, time) {
    const p = drum.player(inst);
    if (!p.loaded) return;
    if (window.dmtLastStart[inst] !== undefined && time <= window.dmtLastStart[inst]) return;
    window.dmtLastStart[inst] = time;
    p.start(time);
  }

  // Beschriftungen der Stop-Buttons kommen (uebersetzt) aus versteckten
  // Spans in dmt_ui(); Fallback englisch.
  function labelFromDom(id, fallback) {
    const el = document.getElementById(id);
    const txt = el ? el.textContent.trim() : "";
    return txt || fallback;
  }

  const stimBtn = document.getElementById("play_stimulus");
  const seqBtn = document.getElementById("play_sequencer");

  const labelPlayStimulus = stimBtn ? stimBtn.innerText : "Play stimulus";
  const labelPlayPattern = seqBtn ? seqBtn.innerText : "Play your pattern";
  const labelStopStimulus = labelFromDom("lbl_stop_stimulus", "Stop stimulus");
  const labelStopPattern = labelFromDom("lbl_stop_pattern", "Stop your pattern");

  // --------------------------------------------------
  // GLOBAL STOP
  // --------------------------------------------------

  window.stopDMT = function () {

    try {
      Tone.Transport.stop();
      Tone.Transport.position = 0;
    } catch (e) {}

    sequencerRunning = false;
    stimulusRunning = false;

    if (window.drumPlayer && window.drumPlayer.part) {
      try {
        window.drumPlayer.part.stop();
      } catch (e) {}
    }

    if (window.dmtIntroPart) {
      try {
        window.dmtIntroPart.stop();
      } catch (e) {}
    }

    if (window.dmtIntroTimer) {
      clearTimeout(window.dmtIntroTimer);
      window.dmtIntroTimer = null;
    }

    const introBtn = document.getElementById("intro-sound-button");
    if (introBtn) introBtn.disabled = false;

    step = 0;

    if (cells.length) {
      for (let r = 0; r < cells.length; r++) {
        for (let c = 0; c < cells[r].length; c++) {
          cells[r][c].classList.remove("playhead");
        }
      }
    }

    if (stimBtn) stimBtn.innerText = labelPlayStimulus;
    if (seqBtn) seqBtn.innerText = labelPlayPattern;

    setButtonState("idle");
    setGridEnabled(true);
  };

  // --------------------------------------------------
  // SEQUENCER LOOP (SAFE SINGLE INSTANCE)
  // --------------------------------------------------

  window.dmtLoopId = Tone.Transport.scheduleRepeat((time) => {

    if (sequencerRunning) {
      for (let r = 0; r < rows; r++) {
        if (matrix[r][step]) {
          playSample(["HiHat", "Snare", "Kick"][r], time);
        }
      }
    }

    for (let r = 0; r < rows; r++) {
      for (let c = 0; c < cols; c++) {
        cells[r][c].classList.remove("playhead");
      }
      cells[r][step].classList.add("playhead");
    }

    step = (step + 1) % cols;

  }, "16n");

  // --------------------------------------------------
  // STIMULUS PLAYER
  // --------------------------------------------------

  class DrumStimulusPlayer {

    constructor() {
      this.part = null;
    }

    load(data) {

      if (this.part) {
        this.part.dispose();
      }

      const stepDur = Tone.Time("16n").toSeconds();

      const events = data.map(row => {
        const stepIndex = row.BeatPositionSixteenth - 1;
        const t = Math.max(0, stepIndex * stepDur);
        return [t, row];
      });

      this.part = new Tone.Part((time, row) => {
        playSample(row.Instrument, time);
      }, events);

      this.part.loop = true;
      this.part.loopEnd = Tone.Time("1m").toSeconds();
    }
  }

  window.drumPlayer = new DrumStimulusPlayer();

  if (window.drumStimulus) {
    window.drumPlayer.load(window.drumStimulus);
  }

  // --------------------------------------------------
  // BUTTONS
  // --------------------------------------------------

  if (stimBtn) {
    stimBtn.onclick = async function () {

      await Tone.start();

      if (stimulusRunning) {
        window.stopDMT();
        return;
      }

      window.stopDMT();

      stimulusRunning = true;

      stimBtn.innerText = labelStopStimulus;

      setButtonState("stimulus");
      setGridEnabled(false);

      window.drumPlayer.part.start(0);
      Tone.Transport.start();
    };
  }

  if (seqBtn) {
    seqBtn.onclick = async function () {

      await Tone.start();

      if (sequencerRunning) {
        window.stopDMT();
        return;
      }

      window.stopDMT();

      sequencerRunning = true;

      seqBtn.innerText = labelStopPattern;

      setButtonState("sequencer");

      Tone.Transport.start();
    };
  }

  // --------------------------------------------------
  // INSTRUKTIONS-SEITEN (Ebenen): Button "Play <Instrument> sound" spielt
  // einen Takt lang 2 Schlaege (auf Zaehlzeit 1 und 3 = Schritt 1 und 9),
  // die Zellen leuchten im Takt auf, danach stoppt die Demo von selbst.
  // --------------------------------------------------

  if (cfg.demo) {

    const demoBtn = document.getElementById("intro-sound-button");
    const demoRow = ["HiHat", "Snare", "Kick"].indexOf(cfg.demo);

    if (demoBtn) {
      demoBtn.onclick = async function () {

        await Tone.start();

        window.stopDMT();

        const stepSec = Tone.Time("16n").toSeconds();

        if (window.dmtIntroPart) {
          try { window.dmtIntroPart.dispose(); } catch (e) {}
        }

        window.dmtIntroPart = new Tone.Part((time, s) => {
          playSample(cfg.demo, time);
          Tone.Draw.schedule(() => {
            const el = cells[demoRow] && cells[demoRow][s];
            if (!el) return;
            el.classList.add("flash");
            setTimeout(() => el.classList.remove("flash"), 200);
          }, time);
        }, [0, 8].map(s => [s * stepSec, s]));

        window.dmtIntroPart.start(0);

        demoBtn.disabled = true;

        Tone.Transport.start();

        window.dmtIntroTimer = setTimeout(() => {
          window.stopDMT();
        }, Tone.Time("1m").toSeconds() * 1000 + 150);
      };
    }
  }

  // --------------------------------------------------
  // STOP ON NEXT
  // --------------------------------------------------

  document.addEventListener("click", function (e) {
    if (e.target && e.target.id === "next") {
      if (window.stopDMT) window.stopDMT();
    }
  });

  // --------------------------------------------------
  // NAV SAFETY
  // --------------------------------------------------

  window.addEventListener("beforeunload", function () {
    if (window.stopDMT) window.stopDMT();
  });

};


function resetSequencer () {
  Shiny.setInputValue("sequencer_state", null);
}
