private
enter
	self isOwnerProcess ifTrue: [
		nestingLevel := nestingLevel + 1.
	] ifFalse: [
		mutex wait.
		ownerProcess := Processor activeProcess.
		nestingLevel := 1.
	].