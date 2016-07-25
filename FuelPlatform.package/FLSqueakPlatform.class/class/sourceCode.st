private-extensions-compiledmethod
sourceCode
	<flExtensionOf: #CompiledMethod>
	"Retrieve or reconstruct the source code for this method."
	| trailer source |
	trailer := self trailer.
	trailer sourceCode ifNotNil: [:code | ^ code ].
	trailer hasSourcePointer ifFalse: [^ self codeForNoSource].

	"Situation normal;  read the sourceCode from the file"
	source := [self getSourceFromFile]
				on: Error
				do: [ :ex | ex return: nil].
	source isEmptyOrNil ifTrue: [^ self codeForNoSource].			
	^source