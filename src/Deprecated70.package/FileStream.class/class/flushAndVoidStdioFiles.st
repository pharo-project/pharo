stdio
flushAndVoidStdioFiles

	StdioFiles ifNotNil: [
		StdioFiles do: [ :file |
			file ifNotNil: [ 
				file isReadOnly ifFalse: [
					[ file flush ]
						on: Error
						do: [ :ex | "care less" ] ] ] ].
	].

	self voidStdioFiles