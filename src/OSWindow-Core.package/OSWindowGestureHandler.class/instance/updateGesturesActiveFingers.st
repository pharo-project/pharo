handling fingers
updateGesturesActiveFingers
	registeredGestures do: [ :gesture | gesture activeFingers: (self fingersFromDevice: gesture deviceId) ]