accessing
highlightSmalltalkContext
	^ highlightSmalltalkContext isNil 
		ifTrue: [nil] 
		ifFalse: [highlightSmalltalkContext glamourValue: self entity]