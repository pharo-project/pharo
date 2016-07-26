printing
printPropertiesOn: aStream
	properties ifNil: [^self].
	properties propertyKeysAndValuesDo:
		[:prop :val|
		aStream crtab; nextPut: $<.
		prop = #on:in:
			ifTrue:
				[prop keywords with: val do:
					[:k :v | aStream nextPutAll: k; space; nextPutAll: v; space]]
			ifFalse:
				[prop = #on
					ifTrue: [aStream nextPutAll: prop; nextPutAll:': '; nextPutAll: val] 
					ifFalse: [aStream nextPutAll: prop; nextPutAll:': '; print: val]]. 
		aStream nextPut: $>]