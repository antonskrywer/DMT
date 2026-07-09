

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

          if (stimulusRunning) return;

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

  const drum = new Tone.Players({
    HiHat: "audio/hihat.wav",
    Snare: "audio/snare.wav",
    Kick: "audio/kick.wav"
  }).toDestination();

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

    step = 0;

    if (cells.length) {
      for (let r = 0; r < cells.length; r++) {
        for (let c = 0; c < cells[r].length; c++) {
          cells[r][c].classList.remove("playhead");
        }
      }
    }

    const stimBtn = document.getElementById("play_stimulus");
    const seqBtn = document.getElementById("play_sequencer");

    if (stimBtn) stimBtn.innerText = "Play stimulus";
    if (seqBtn) seqBtn.innerText = "Play your pattern";

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
          const inst = ["HiHat", "Snare", "Kick"][r];
          drum.player(inst).start(time);
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
        drum.player(row.Instrument).start(time);
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

  const stimBtn = document.getElementById("play_stimulus");
  const seqBtn = document.getElementById("play_sequencer");

  if (stimBtn) {
    stimBtn.onclick = async function () {

      await Tone.start();

      if (stimulusRunning) {
        window.stopDMT();
        return;
      }

      window.stopDMT();

      stimulusRunning = true;

      stimBtn.innerText = "Stop stimulus";

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

      seqBtn.innerText = "Stop your pattern";

      setButtonState("sequencer");

      Tone.Transport.start();
    };
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
