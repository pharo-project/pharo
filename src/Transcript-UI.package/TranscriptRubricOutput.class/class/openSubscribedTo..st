construction
openSubscribedTo: aTranscript

	| model |
	
	model := self new.	
	aTranscript addOutput: model.
	
	^model open
	
	
	