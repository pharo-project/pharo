dependencies
addDependent: anObject
	"Make the given object one of the receiver's dependents."

	| dependents |
	dependents := self dependents.
	(dependents includes: anObject) ifFalse:
		[self myDependents: (dependents copyWithDependent: anObject)].
	^ anObject