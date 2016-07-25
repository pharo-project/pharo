source pointers
stampAtPointer	

	"A RGMethodDefinition that was set as historical will retrieve the stamp using the sourcePointer"

	^ self sourcePointer notNil
		ifTrue: [ SourceFiles timeStampAt: self sourcePointer ]
		ifFalse:[ nil ]