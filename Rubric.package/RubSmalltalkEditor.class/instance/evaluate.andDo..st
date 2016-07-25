do-its
evaluate: aStream andDo: aBlock
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result."
	| result rcvr ctxt |
	(self model respondsTo: #doItReceiver) 
		ifTrue: [ rcvr := self model doItReceiver.
				ctxt := self model doItContext]
		ifFalse: [ rcvr := ctxt := nil].
	result := rcvr class compiler
			source: aStream;
			context: ctxt;
			receiver: rcvr;
			requestor: self morph  "don't set it to self.. The receiver can be destroyed as a result of evaluateSelection";
			failBlock:  [self morph flash. ^ nil];
			evaluate.		
			
	^ aBlock value: result