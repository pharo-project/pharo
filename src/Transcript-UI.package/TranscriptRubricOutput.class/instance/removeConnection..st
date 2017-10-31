private
removeConnection: aTranscript

	connections remove: aTranscript.
	
	aTranscript removeOutput: self.