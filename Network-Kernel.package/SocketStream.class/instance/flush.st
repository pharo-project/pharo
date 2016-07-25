control
flush
	"If the other end is connected and we have something
	to send, then we send it and reset the outBuffer."

	((outNextToWrite > 1) and: [socket isOtherEndClosed not])
		ifTrue: [
			[socket sendData: outBuffer count: outNextToWrite - 1]
				on: ConnectionTimedOut
				do: [:ex | shouldSignal ifFalse: ["swallow"]].
			outNextToWrite := 1]