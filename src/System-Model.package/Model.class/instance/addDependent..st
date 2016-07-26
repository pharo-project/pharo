dependents
addDependent: anObject
	"Make the given object one of the receiver's dependents."

	(self dependents includes: anObject) ifFalse:
		[self myDependents: (self dependents copyWithDependent: anObject)].
	
	^ anObject