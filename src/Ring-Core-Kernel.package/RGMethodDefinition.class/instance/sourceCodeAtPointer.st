source pointers
sourceCodeAtPointer

	"A RGMethodDefinition that was set as historical will retrieve the sourceCode using the sourcePointer"

	^ self sourcePointer notNil
		ifTrue: [ SourceFiles sourceCodeAt: self sourcePointer ]
		ifFalse:[ nil ]