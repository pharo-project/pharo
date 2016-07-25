do-its
debugIt

	| method receiver context |
	(model respondsTo: #doItReceiver) 
		ifTrue: 
			[receiver := model doItReceiver.
			context := model doItContext]
		ifFalse:
			[receiver := context := nil].
	self lineSelectAndEmptyCheck: [^self].
	method := self compileSelectionFor: receiver in: context.
	method notNil ifTrue:
		[self debug: method receiver: receiver in: context].