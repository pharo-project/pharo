Extension { #name : #Trait }

{ #category : #'*ston-core' }
Trait >> stonOn: stonWriter [
	stonWriter 
		writeObject: self 
		listSingleton: self name asSymbol
]
