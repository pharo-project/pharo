running
check: aNode forCritiquesDo: aBlock
	aNode isSequence ifTrue: [ 
		aNode temporaries do: [ :temp |
			self
				checkTemp: temp
				followedBy: aNode statements 
				forCritiquesDo: aBlock ] ]