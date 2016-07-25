do-its
debug: aStream

	| method receiver context |
	(self model respondsTo: #doItReceiver) 
		ifTrue: 
			[receiver := self model doItReceiver.
			context := self model doItContext]
		ifFalse:
			[receiver := context := nil].
	method := self compile: aStream for: receiver in: context.
	method notNil ifTrue: [
		self debug: method receiver: receiver in: context ].
	