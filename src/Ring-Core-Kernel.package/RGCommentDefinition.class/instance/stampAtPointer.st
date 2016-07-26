source pointers
stampAtPointer
	"A RGMethodDefinition may be created to point the sourceFile in which case it retrieves the stamp"

	^ self sourcePointer notNil
		ifTrue: [ SourceFiles timeStampAt: self sourcePointer for: self commentDataPointers ]
		ifFalse:[ nil ]	