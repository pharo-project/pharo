filein/out
storeBitsOn:aStream base:anInteger
	bits do: [:word | 
		anInteger = 10
			ifTrue: [aStream space]
			ifFalse: [aStream crtab: 2].
		word storeOn: aStream base: anInteger].
