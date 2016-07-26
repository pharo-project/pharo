canvas
canvas: x 
	canvas := x.
	damageRecorder 
		ifNil: [damageRecorder := DamageRecorder new]
		ifNotNil: [damageRecorder doFullRepaint]