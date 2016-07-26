*renraku
check: anEntity forCriticsDo: aCriticBlock 
	self 
		deprecated: 'Use check:forCritiquesDo: instead'
		on: 	'24 May 2016' 
		in: 'Pharo-6.0-60028-'.
	^ self check: anEntity forCritiquesDo: aCriticBlock 