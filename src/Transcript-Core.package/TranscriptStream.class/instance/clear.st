streaming
clear 

	self critical: [ 
		outputs do: [ :each | each transcriptCleared: self ].
		stream reset.
		self applyUpdateStrategy
	]