private
setSender: newSender receiver: newReceiver method: newMethod arguments: arguments 
	"Create the receiver's initial state."

	sender := newSender.
	receiver := newReceiver.
	method := newMethod.
	closureOrNil := nil.
	pc := method initialPC.
	self stackp: method numTemps.
	1 to: arguments size do: [ :i | 
		self at: i put: (arguments at: i)]