do-its
evaluateSelectionAndDo: aBlock
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result."
	| result rcvr ctxt |

	self lineSelectAndEmptyCheck: [^ ''].
	(model respondsTo: #doItReceiver) 
		ifTrue: [ rcvr := model doItReceiver.
				ctxt := model doItContext]
		ifFalse: [ rcvr := ctxt := nil].
	result := rcvr class compiler
			source: self selectionForDoitAsStream;
			context: ctxt;
			receiver: rcvr;
			requestor: self morph  "don't set it to self.. The receiver can be destroyed as a result of evaluateSelection";
			failBlock:  [self morph flash. ^ nil];
			evaluate.		
			
	^ aBlock value: result