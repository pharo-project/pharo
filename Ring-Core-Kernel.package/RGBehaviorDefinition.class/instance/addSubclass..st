subclassing
addSubclass: aRGBehaviorDefinition
	"Adds a direct subclass of the receiver"

	(self subclasses includes: aRGBehaviorDefinition)
		ifFalse:[ self subclasses add: aRGBehaviorDefinition ]