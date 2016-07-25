report
printOn: aStream total: total totalTime: totalTime tallyExact: isExact 
	| className myTally aClass percentage |
	isExact ifTrue:[
		myTally := tally.
		receivers ifNotNil: [ receivers asArray do: [ :r | myTally := myTally - r tally ] ].
		aStream print: myTally; space.
	] ifFalse:[
		percentage := tally asFloat / total * 100.0 roundTo: 0.1.
		aStream print: percentage; nextPutAll: ' (';
				nextPutAll: (percentage * totalTime / 100) rounded asStringWithCommas;
				nextPutAll: ')  '.
	].
	self isPrimitives ifTrue:[
		aStream nextPutAll: 'primitives'; cr
	] ifFalse:[
		aClass := method methodClass.
		className := aClass name contractTo: self maxClassNameSize.
		aStream nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['  ']
							ifFalse: [' [' , aClass name , ']  ']);
				nextPutAll: (method selector
							contractTo: self maxClassPlusSelectorSize - className size);
				cr.
	].