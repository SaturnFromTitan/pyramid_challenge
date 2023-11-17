;(function SoundManager() {
  const sounds = {
    countdown: new Howl({ src: ['assets/sounds/countdown.wav'] }),
  };

  window.SoundManager = function (name) {
    const sound = sounds[name];
    console.assert(sound != null, `Unknown sound: ${name}`);
    sound.play();
  };
}());
