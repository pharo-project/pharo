*renraku
check: anEntity ifNoCritics: alternativeBlock 
	self 
		deprecated: 'Use check:ifNoCritiques: instead'
		on: 	'24 May 2016' 
		in: 'Pharo-6.0-60028-'.
	^ self check: anEntity ifNoCritiques: alternativeBlock 