printing
printOn: aStream
	| selectorNode |
	selectorNode := self selectorNode.
	precedence = 1
		ifTrue:
			[selectorNode isForFFICall
				ifTrue: [selectorNode
							printAsFFICallWithArguments: arguments
							on: aStream
							indent: 0]
				ifFalse: [aStream nextPutAll: selectorNode key]]
		ifFalse:
			[selectorNode key keywords with: arguments do:
				[:kwd :arg |
				aStream nextPutAll: kwd; space; nextPutAll: arg key; space]].
	comment == nil ifFalse:
		[aStream crtab: 1.
		 self printCommentOn: aStream indent: 1].
	block printTemporaries: temporaries on: aStream doPrior: [aStream crtab: 1].
	primitive > 0 ifTrue:
		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"
			[aStream crtab: 1.
			 self printPrimitiveOn: aStream]].
	self printPropertiesOn: aStream.
	self printPragmasOn: aStream.
	aStream crtab: 1.
	block printStatementsOn: aStream indent: 0