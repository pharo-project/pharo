private
prepareToTrackCenterOfRotation: evt with: rotationHandle
	evt hand obtainHalo: self.
	evt shiftPressed ifTrue:[
		self removeAllHandlesBut: rotationHandle.
	] ifFalse:[
		rotationHandle setProperty: #dragByCenterOfRotation toValue: true.
		self startDrag: evt with: rotationHandle
	].
	evt hand showTemporaryCursor: Cursor blank