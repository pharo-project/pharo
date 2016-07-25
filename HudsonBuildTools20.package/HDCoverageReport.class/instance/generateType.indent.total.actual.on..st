generating
generateType: aString indent: anInteger total: totalInteger actual: actualInteger on: aStream
	aStream tab: anInteger; 
		nextPutAll: '<coverage type="'; nextPutAll: aString; 
		nextPutAll: ', %" value="'; print: (totalInteger = 0 ifTrue: [ 0 ] ifFalse: [ (100.0 * actualInteger / totalInteger) rounded ]); 
		nextPutAll: '% ('; print: actualInteger; nextPut: $/; print: totalInteger; nextPutAll: ')"/>'; 
		nextPut: Character lf