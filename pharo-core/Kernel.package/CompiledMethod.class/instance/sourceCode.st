accessing
sourceCode
	"Retrieve or reconstruct the source code for this method."

	| trailer |
	trailer := self trailer.
	trailer sourceCode ifNotNil: [:code | ^ code ].
	trailer hasSourcePointer ifFalse: [ ^ self codeForNoSource ].

	^ self getSourceFromFile ifEmpty: [ self codeForNoSource ]