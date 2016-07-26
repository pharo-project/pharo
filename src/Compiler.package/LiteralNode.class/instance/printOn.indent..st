printing
printOn: aStream indent: level

	key isVariableBinding
		ifTrue:
			[key key 
				ifNil:
					[aStream nextPutAll: '###'; nextPutAll: key value soleInstance name]
				ifNotNil:
					[aStream nextPutAll: '##'; nextPutAll: key key]]
		ifFalse:
			[key storeOn: aStream]