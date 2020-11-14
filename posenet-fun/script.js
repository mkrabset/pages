var video;

function init() {
  video = document.querySelector("#videoElement");


  if (navigator.mediaDevices.getUserMedia) {
    navigator.mediaDevices.getUserMedia({ video: true })
      .then(function (stream) {
        video.srcObject = stream;
      })
      .catch(function (err0r) {
        console.log("Something went wrong!");
      });
  }

  addFrameListener()
}

function addFrameListener() {
  posenet.load({
    architecture: 'MobileNetV1',
    outputStride: 16,
    inputResolution: { width: 640, height: 480 },
    multiplier: 0.75
  }).then((net) => {
    video.addEventListener('timeupdate', function () {
      var imageElement = document.getElementById('videoElement')

      net.estimateMultiplePoses(imageElement, {
        flipHorizontal: true,
        maxDetections: 2,
        scoreThreshold: 0.6
      }).then(function (poses) {
        draw(poses);
      })

    })
  })
}

function findPart(partName, pose) {
  return pose.keypoints.find((kp) => kp.part === partName)
}

function findPos(partName, pose) {
  const part = findPart(partName, pose)
  return [part.position.x, part.position.y]
}

var ra = 0;
var la = 0;
var rav = 0;
var lav = 0;

function draw(poses) {
  var canvas = document.querySelector("#cv1");
  var ctx = canvas.getContext("2d");
  ctx.clearRect(0, 0, 640, 480)
  poses.forEach(pose => drawPose(ctx, pose))
}

function drawPose(ctx, pose) {
  const leftEye = findPos('leftEye', pose)
  const rightEye = findPos('rightEye', pose)
  const dx = leftEye[0] - rightEye[0];
  const dy = leftEye[1] - rightEye[1];
  const dist = Math.sqrt(dx * dx + dy * dy)
  const scale=dist/50

  drawEye(ctx, ...leftEye, ra, scale);
  drawEye(ctx, ...rightEye, la,scale);
}

function drawEye(ctx, x, y, a, scale) {
  ctx.beginPath();
  ctx.fillStyle = "#ffffff";
  ctx.arc(x, y, scale*18, 0, 2 * Math.PI);
  ctx.fill();
  ctx.stroke();

  ctx.beginPath();
  pupil = [x + scale*8 * Math.cos(a), y + scale*8 * Math.sin(a)]
  ctx.fillStyle = "#000000";
  ctx.arc(...pupil, scale*8, 0, 2 * Math.PI);
  ctx.fill();
  updatePupils();
}

function updatePupils() {
  rav += (Math.random() - 0.5) / 2
  lav += (Math.random() - 0.5) / 2
  rav *= 0.95
  lav *= 0.95
  ra += rav;
  la += lav;
}

