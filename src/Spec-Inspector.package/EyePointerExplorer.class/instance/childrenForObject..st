api
childrenForObject: anObject
	| objects  |
	objects := anObject wrappedObject pointersToExcept: { self. anObject}.
	"filter out all wrappers and (otionally) all those who pointing only weakly to subject"
	objects := objects reject: [ :each | each class =  EyePointerWrapper].
	onlyStrong ifTrue: [objects := objects reject: [ :each | each pointsOnlyWeaklyTo: anObject wrappedObject]]. 
	^ objects collect: [ :each |
		(PointerEyeElement
			host: anObject wrappedObject 
			value: each)]