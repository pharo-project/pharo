Extension { #name : #TApplyingOnClassSide }

{ #category : #'*ston-core' }
TApplyingOnClassSide >> stonName [
	^ #Class
]

{ #category : #'*ston-core' }
TApplyingOnClassSide >> stonOn: stonWriter [
	stonWriter
		writeObject: self 
		listSingleton: self instanceSide name asSymbol
]
