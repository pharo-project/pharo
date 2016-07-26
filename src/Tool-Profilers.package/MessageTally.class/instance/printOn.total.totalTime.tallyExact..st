printing
printOn: aStream total: total totalTime: totalTime tallyExact: isExact 

	isExact 
		ifTrue: [
			| myTally |
			myTally := tally.
			receivers == nil 
				ifFalse: [receivers do: [:r | myTally := myTally - r tally]].
			aStream
				print: myTally;
				space]
		ifFalse: [
			| percentage |
			percentage := tally asFloat / total * 100.0.
			aStream
				nextPutAll: (percentage printShowingDecimalPlaces: 1);
				nextPutAll: '% {';
				print: (percentage * totalTime / 100) rounded;
				nextPutAll: 'ms} '].
	receivers 
		ifNil: [
			aStream
				nextPutAll: 'primitives';
				cr]
		ifNotNil: [
			| className aSelector aClass |
			aSelector := method selector.
			aClass := method methodClass.
			className := aClass name contractTo: self maxClassNameSize.
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: (aSelector 
							contractTo: self maxClassPlusSelectorSize - className size);
				cr]