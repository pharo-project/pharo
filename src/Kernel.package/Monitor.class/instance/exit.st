private
exit
	nestingLevel := nestingLevel - 1.
	nestingLevel < 1 ifTrue: [
		ownerProcess := nil.
		mutex signal
	].