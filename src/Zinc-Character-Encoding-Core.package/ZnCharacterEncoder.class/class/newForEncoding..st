instance creation
newForEncoding: string
	"Return a new character encoder object for an encoding described by string.
	Search for a subclass that handles it and delegate (subclassResponsibility)."
	
	| concreteSubclass |
	concreteSubclass := self allSubclasses 
		detect: [ :each | each handlesEncoding: string ] 
		ifNone: [ ^ self default ].
	^ concreteSubclass newForEncoding: string