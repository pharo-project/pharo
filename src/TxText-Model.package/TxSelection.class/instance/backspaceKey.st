operations
backspaceKey
	"behave as if user pressed the Backspace key"
	
	self isEmpty ifTrue: [ 
		self beSelection. 
		start moveLeft ].
	self cut