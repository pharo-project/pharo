printing
printOn: aStream indent: level precedence: p 

	p > 0 ifTrue: [aStream nextPut: $(].
	messages first printReceiver: receiver on: aStream indent: level.
	1 to: messages size do: 
		[:i | (messages at: i) printOn: aStream indent: level.
		i < messages size ifTrue: 
				[aStream nextPut: $;.
				messages first precedence >= 2 ifTrue: [aStream crtab: level + 1]]].
	p > 0 ifTrue: [aStream nextPut: $)]