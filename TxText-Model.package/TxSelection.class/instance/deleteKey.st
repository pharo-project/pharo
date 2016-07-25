operations
deleteKey
	"behave as if user pressed the Delete key"
	
	self isEmpty ifTrue: [ 
		self beSelection. 
		end moveRight ].
	self cut