printing
storeOn: aStream 
	| classBlock |
	aStream
		nextPutAll: '((';
		nextPutAll: self class name;
		nextPutAll: ' onEnvironment: '.
	environment storeOn: aStream.
	aStream
		nextPut: $);
		nextPutAll: ' classes: #('.
	classBlock := 
			[:key :value | 
			aStream
				nextPutAll: '#(';
				nextPutAll: key;
				nextPutAll: ' #('.
			value do: 
					[:each | 
					aStream
						nextPutAll: each;
						nextPut: $ ].
			aStream
				nextPutAll: '))';
				cr].
	classSelectors keysAndValuesDo: classBlock.
	aStream nextPutAll: ') metaClasses: #('.
	metaClassSelectors keysAndValuesDo: classBlock.
	aStream nextPutAll: '))'