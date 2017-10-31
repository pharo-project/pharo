streaming
endEntry

  	self critical: [ 
		outputs do: [ :each |  
			each transcriptEnded: self withEntry: stream contents
		 ].
		stream resetContents.
		self applyUpdateStrategy.
	].