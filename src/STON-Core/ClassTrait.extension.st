Extension { #name : #ClassTrait }

{ #category : #'*ston-core' }
ClassTrait >> stonName [
	^ #Class
]

{ #category : #'*ston-core' }
ClassTrait >> stonOn: stonWriter [
	stonWriter
		writeObject: self 
		listSingleton: self theNonMetaClass name asSymbol
]
