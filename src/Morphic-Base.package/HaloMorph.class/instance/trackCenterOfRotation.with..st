private
trackCenterOfRotation: anEvent with: rotationHandle
	(rotationHandle hasProperty: #dragByCenterOfRotation) 
		ifTrue:[^self doDrag: anEvent with: rotationHandle].
	anEvent hand obtainHalo: self.
	rotationHandle center: anEvent cursorPoint.