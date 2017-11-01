Extension { #name : #String }

{ #category : #'*OSWindow-Core' }
String >> asUTF8Bytes [
	^ ZnUTF8Encoder new encodeString: self
]
