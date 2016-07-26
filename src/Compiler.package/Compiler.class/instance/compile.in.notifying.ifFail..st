public - old
compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock 
	^self compile: textOrStream in: aClass classified: nil notifying: aRequestor ifFail: failBlock 