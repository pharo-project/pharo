canvas converting
asShadowDrawingCanvas
	self flag: #todo.	"this may not work, but it
doesn't hurt yet and HandMorph relies on this"
	^ (ShadowDrawingCanvas on: self) shadowColor: (Color orange alpha: 0.2)