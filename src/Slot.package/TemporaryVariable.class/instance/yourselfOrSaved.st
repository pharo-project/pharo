saved temps
yourselfOrSaved
	method savedTemps ifNil: [ ^self  ].	
	^method savedTemps 
		detect:  	[ :each | each = self ]
		ifFound: [ :found | found ]
		ifNone: 	[ self ]