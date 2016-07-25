source pointers
protocolAtPointer

	"A RGMethodDefinition that was set as historical will retrieve the protocol using the sourcePointer"

	^ self sourcePointer notNil
		ifTrue: [ SourceFiles protocolAt: self sourcePointer ]
		ifFalse:[ nil ]