printing
printStatementsOn: aStream indent: levelOrZero
	| len shown thisStatement level |
	level := 1 max: levelOrZero.
	comment == nil
		ifFalse: 
			[self printCommentOn: aStream indent: level.
			aStream crtab: level].
	len := shown := statements size.
	(levelOrZero = 0 "top level" and: [statements last isReturnSelf])
		ifTrue: [shown := 1 max: shown - 1]
		ifFalse: ["should a trailing nil be printed or not? Not if it is an implicit result."
				(arguments size = 0
				and: [len >= 1
				and: [(statements at: len) == NodeNil
				and: [len = 1
					or: [len > 1
						and: [(statements at: len - 1) isMessageNode
						and: [(statements at: len - 1) isNilIf]]]]]])
					ifTrue: [shown := shown - 1]].
	1 to: shown do: 
		[:i | 
		thisStatement := statements at: i.
		thisStatement printOn: aStream indent: level.
		i < shown ifTrue: [aStream nextPut: $.; crtab: level].
		(thisStatement comment ~~ nil and: [thisStatement comment size > 0])
			ifTrue: 
				[i = shown ifTrue: [aStream crtab: level].
				thisStatement printCommentOn: aStream indent: level.
				i < shown ifTrue: [aStream crtab: level]]]