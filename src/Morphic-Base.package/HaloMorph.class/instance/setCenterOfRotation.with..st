private
setCenterOfRotation: evt with: rotationHandle
	| localPt |
	evt hand obtainHalo: self.
	evt hand showTemporaryCursor: nil.
	(rotationHandle hasProperty: #dragByCenterOfRotation) ifFalse:[
		localPt := innerTarget transformFromWorld globalPointToLocal: rotationHandle center.
		innerTarget setRotationCenterFrom: localPt.
	].
	rotationHandle removeProperty: #dragByCenterOfRotation.
	self endInteraction
