do-its
evaluateSelectionAndDo: aBlock
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result."
	| result rcvr ctxt |
	self lineSelectAndEmptyCheck: [^ nil].

	(model respondsTo: #doItReceiver) 
		ifTrue: [ rcvr := model doItReceiver.
				ctxt := model doItContext]
		ifFalse: [rcvr := ctxt := nil].
	result := rcvr class compiler
			source: self selectionAsStream;
			context: ctxt;
			receiver: rcvr;
			requestor: self;
			failBlock:  [morph flash. ^ nil];
			evaluate.		
	^aBlock value: result