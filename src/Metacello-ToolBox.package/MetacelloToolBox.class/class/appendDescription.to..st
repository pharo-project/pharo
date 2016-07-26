private
appendDescription: aString to: versionSpec
	aString isEmpty
		ifTrue: [ ^ self ].
	^ versionSpec description value isEmpty
		ifTrue: [ versionSpec description: aString ]
		ifFalse: [ 
			| strm |
			strm := WriteStream on: String new.
			strm
				nextPutAll: versionSpec description value;
				cr;
				nextPutAll: aString.
			versionSpec description: strm contents ]