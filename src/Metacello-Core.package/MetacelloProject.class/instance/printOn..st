printing
printOn: aStream
	| label |
	self configuration class printOn: aStream.
	aStream nextPut: $(.
	self versions
		do: [ :vrsn | 
			aStream nextPutAll: vrsn versionString.
			vrsn spec ~~ nil
				ifTrue: [ 
					(label := vrsn spec projectLabel) isEmpty
						ifFalse: [ aStream nextPutAll: ' [' , label , ']' ] ].
			aStream
				nextPut: $,;
				space ].
	aStream nextPut: $)