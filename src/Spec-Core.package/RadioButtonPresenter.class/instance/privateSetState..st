private
privateSetState: aBoolean
	"Should only be used by RadioButtonGroup"
	
	stateHolder value: aBoolean.
	self changed: #state