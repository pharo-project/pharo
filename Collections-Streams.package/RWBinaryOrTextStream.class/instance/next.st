accessing
next

	| byte |
	^ isBinary 
			ifTrue: [byte := super next.
				 byte ifNil: [nil] ifNotNil: [byte asciiValue]]
			ifFalse: [super next].
