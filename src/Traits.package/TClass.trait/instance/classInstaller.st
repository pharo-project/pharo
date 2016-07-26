subclass creation
classInstaller
	"Answer the class responsible of creating subclasses of myself in the system."
	^self isAnonymous 
		ifTrue: [Smalltalk anonymousClassInstaller]
		ifFalse: [Smalltalk classInstaller]