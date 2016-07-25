*renraku
check: anEntity forCriticsDo: aCriticBlock ifNone: alternativeBlock 
	self 
		deprecated: 'Use check:forCritiquesDo:ifNone: instead'
		on: 	'24 May 2016' 
		in: 'Pharo-6.0-60028-'.
	^ self check: anEntity forCritiquesDo: aCriticBlock ifNone: alternativeBlock 