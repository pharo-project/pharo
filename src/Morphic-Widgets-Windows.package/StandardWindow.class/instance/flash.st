operations
flash
	"Flash the window."
	
	(self hasProperty: #activeModalFlash) ifTrue: [ ^self ].
	self setProperty: #activeModalFlash toValue: true.
		
	[ 
		
		| oldColor newColor duration |
	
		oldColor := self fillStyle.
		newColor := self color alphaMixed: 0.5 with: Color white.
		duration := 0.
	
		2 timesRepeat: [
			duration milliSeconds asDuration wait.
			duration := 200.
			
			self color: newColor.
			
			duration milliSeconds asDuration wait.
			
			self color = newColor ifFalse: [ 
				"the color changed while waiting (theme changed)"
				oldColor := self fillStyle 
			].
		
			self fillStyle: oldColor.
		].
	
		self removeProperty: #activeModalFlash
	
	] fork.
