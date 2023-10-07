Extension { #name : 'MessageTally' }

{ #category : '*ProfilerUI' }
MessageTally >> asTallyModel [
	^ TallyModelTime new
		item: self;
		yourself
]
