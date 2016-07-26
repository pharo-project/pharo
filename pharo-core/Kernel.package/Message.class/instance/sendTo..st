sending
sendTo: receiver
	"answer the result of sending this message to receiver"

	^ receiver perform: selector withArguments: args