correcting
declareInstVar: name
	"Declare an instance variable."
	self methodClass addInstVarNamed: name.
	"We are changing a class after the scope hierarchy was created, so we need to update the
	Instance Scope"
	self methodNode scope instanceScope slots: self methodClass allSlots.
	^ self lookUpVariable