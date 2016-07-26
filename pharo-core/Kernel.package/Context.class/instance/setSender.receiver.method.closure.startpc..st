private
setSender: newSender receiver: newReceiver method: newMethod closure: newClosure startpc: startpc
	"Create the receiver's initial state."

	sender := newSender.
	receiver := newReceiver.
	method := newMethod.
	closureOrNil := newClosure.
	pc := startpc.
	stackp := 0