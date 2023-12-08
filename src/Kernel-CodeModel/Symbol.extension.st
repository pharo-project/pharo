Extension { #name : 'Symbol' }

{ #category : '*Kernel-CodeModel' }
Symbol >> => aVariable [
	"If the slot we give as argument is not present in the image, we create an UndefinedSlot with the AST of the slot definition"

	^ aVariable
		ifNil: [ UndefinedSlot named: self ast: thisContext sender sourceNodeExecuted]
		ifNotNil: [ aVariable named: self ]
]
