updating
applyUpdateFor: aTranscript

	self wantsToUpdate ifFalse: [ ^self ].

	aTranscript outputs do: [ :each | each transcriptUpdate: self ].
	
	self subscriptionsUpdated 