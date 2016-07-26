as yet unclassified
doAutoAccept: evt
	"Do the key stroke and check to see if it should be accepted."

	self autoAccept == true 
		ifTrue: [self editView hasUnacceptedEdits 
			ifTrue: [self editor accept]]