running
checkTemp: aTemp followedBy: statements forCritiquesDo: aBlock
	(self checkTemp: aTemp followedBy: statements) ifFalse: [ 
		aBlock cull: (self critiqueFor: aTemp) ]