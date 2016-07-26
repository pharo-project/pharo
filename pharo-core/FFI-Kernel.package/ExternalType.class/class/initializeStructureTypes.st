class initialization
initializeStructureTypes
	"ExternalType initialize"
	| referentClass pointerType |
	self cleanupUnusedTypes.
	StructTypes keysAndValuesDo:[:referentName :type|
		referentClass := (Smalltalk at: referentName ifAbsent:[nil]).
		(referentClass isBehavior and:[
			referentClass includesBehavior: ExternalStructure])
				ifFalse:[referentClass := nil].
		type compiledSpec: 
			(WordArray with: self structureSpec).
		type newReferentClass: referentClass.
		pointerType := type asPointerType.
		pointerType compiledSpec: 
			(WordArray with: self pointerSpec).
		pointerType newReferentClass: referentClass.
	].