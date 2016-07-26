printing
printOn: aStream indent: level 
	aStream nextPutAll: (key == nil
							ifTrue: ['<key==nil>']
							ifFalse: [key])