writing
encodeList: elements
	writeStream nextPut: $[.
	elements isEmpty
		ifTrue: [
			self prettyPrintSpace ]
		ifFalse: [
			self indentedDo: [
				self newlineIndent.
				elements 
					do: [ :each | self nextPut: each ]
					separatedBy: [ self listElementSeparator ] ].
			self newlineIndent ].
	writeStream nextPut: $]
