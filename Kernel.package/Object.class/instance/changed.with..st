updating
changed: anAspect with: anObject
	"Receiver changed. The change is denoted by the argument anAspect. 
	Usually the argument is a Symbol that is part of the dependent's change 
	protocol. Inform all of the dependents. Also pass anObject for additional information."

	self dependents do: [:aDependent | aDependent update: anAspect with: anObject]