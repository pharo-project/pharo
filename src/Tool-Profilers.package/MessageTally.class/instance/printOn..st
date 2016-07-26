printing
printOn: aStream 
	| aSelector className |
	(class isNil or: [method isNil]) ifTrue: [^super printOn: aStream].
	aSelector := method selector.
	className := method methodClass name.
	aStream
		nextPutAll: (class name contractTo: self maxClassNameSize);
		nextPutAll: '(';
		nextPutAll: (className  contractTo: self maxClassNameSize);
		nextPutAll: ')';
		nextPutAll: ' >> ';
		nextPutAll: (aSelector 
					contractTo: self maxClassPlusSelectorSize - className size)