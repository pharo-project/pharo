Extension { #name : #TClass }

{ #category : #'*ston-core' }
TClass >> stonOn: stonWriter [
	stonWriter 
		writeObject: self 
		listSingleton: self name asSymbol
]
