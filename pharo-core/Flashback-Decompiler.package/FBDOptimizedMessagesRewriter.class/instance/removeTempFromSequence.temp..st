private
removeTempFromSequence: seq temp: temp
	| temps |
	seq ifNil: [ ^self "2 to:do: in the same method, temp has already  been removed" ].
	seq isSequence ifFalse: [ ^ self removeTempFromSequence: seq parent temp: temp ].
	temps := seq temporaries reject: [ :tmp | tmp = temp ].
	temps size = seq temporaries size
		ifTrue: [ self removeTempFromSequence: seq parent temp: temp ]
		ifFalse: [ seq temporaries: temps ]