converting
asMessageSend
	^MessageSend receiver: self receiver selector: selector arguments: (Array withAll: self arguments) 
