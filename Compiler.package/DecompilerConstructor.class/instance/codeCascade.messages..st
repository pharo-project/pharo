constructor
codeCascade: receiver messages: messages

	^ (BraceNode new matchBraceStreamReceiver: receiver messages: messages)
		ifNil: [CascadeNode new receiver: receiver messages: messages]