visual properties
updateVisualState: evt
	
	oldColor ifNotNil: [
		 self color: 
			((self containsPoint: evt cursorPoint)
				ifTrue: [oldColor mixed: 0.5 with: Color white]
				ifFalse: [oldColor])]
