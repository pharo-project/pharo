converting
asWeakMessageSend

	^ WeakMessageSend
		receiver: receiver
		selector: selector
		arguments: arguments copy