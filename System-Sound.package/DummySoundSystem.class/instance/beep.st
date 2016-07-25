beep
beep
	"Make a primitive beep if possible."

	self soundEnabled 
		ifTrue: [ self beepPrimitive ]