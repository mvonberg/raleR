# JAVASCRIPT to initiate an array from an vector
JS_create_array <- function(values,array_name) {
  shiny::tags$script(paste0("var ",array_name," = [",paste0("'",values,"'",collapse=","),"];"))
}

# JAVASCRIPT to let a slider change an audio object and play new sound on value change. Also stores the slider input value.
JS_processAudioSliderInput <- function() {
  shiny::tags$script(shiny::HTML(
    "var slider = document.getElementById('slider');

    function export_answer(value) {

        Shiny.onInputChange('slider', value);

    }

    function processAudioSliderInput() {
      var btnSlider = document.getElementById('btnsliderAudio')
      var btnTarget = document.getElementById('btntarget')

      var sliderVal = slider.value;
      export_answer(sliderVal);
      var source = document.getElementById('sliderAudioSource');
      if (snd.played && snd != document.getElementById('sliderAudio')) {
        fadeOut(snd);
        snd.volume=1;
      }
      source.src = sliderSounds[sliderVal-sliderOffset];
      snd = document.getElementById('sliderAudio');
      snd.load();
      playSound(btnSlider);
    }

    export_answer(slider.value);
    slider.addEventListener('change',processAudioSliderInput);
    "
  )
  )
}

# JAVASCRIPT to handle audio toggling on the client side.
#
# CASE 1: previous SND matches new stimulus: if playback is active, pause and reset time else play.
# CASE 2: new (or first) stimulus is selected: pause playback if active and reset time, then assign SND to new stimulus and start playing

JS_toggleSounds <- function() {
  shiny::tags$script(shiny::HTML(
    "
    var snd = 0; // global variable to track the currently playing audio

    function toggleSounds(stimulus) {
      var btn = document.getElementById('btn' + stimulus);
      if (snd==document.getElementById(stimulus)) {
        if (snd.paused) {
          playSound(btn);
        } else {
          fadeOut(snd);
          //btn.classList.remove('playing');
        }
      } else {
        if (snd.played) {
          fadeOut(snd);
        }
        snd = document.getElementById(stimulus);
        //snd.play();
        playSound(btn);
      }
    }

    function fadeOut(audio, callback) {
      let fadeInterval = setInterval(() => {
        if (audio.volume > 0.1) {
          audio.volume -= 0.1;
        } else {
          audio.volume = 0;
          clearInterval(fadeInterval);
          audio.pause();
          audio.currentTime = 0;
          audio.volume = 1; // reset
          if (callback) callback();
        }
      }, 20); // total duration: ~200ms
    }


    async function playSound(button) {
      try {
        await snd.play();
        console.log('hello');
      } catch (err) {
        console.log('goodbye');
      }
    }
    "
  ))
}
