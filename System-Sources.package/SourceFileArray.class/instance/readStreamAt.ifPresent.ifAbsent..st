private - accessing file streams
readStreamAt: sourcePointer ifPresent: presentBlock ifAbsent: absentBlock

	| index position |
	sourcePointer ifNil: [ ^ absentBlock value ].
	
	index := self fileIndexFromSourcePointer: sourcePointer.
	index = 0 ifTrue: [ ^ absentBlock value ].	"no source pointer for this element"

	position := self filePositionFromSourcePointer: sourcePointer.
	position = 0 ifTrue: [ ^ absentBlock value ].

	^ self 
		readStreamAtFileIndex: index 
		atPosition: position 
		ifPresent: presentBlock 
		ifAbsent: absentBlock