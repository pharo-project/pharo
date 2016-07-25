public - old
evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock
	^ self evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: false.