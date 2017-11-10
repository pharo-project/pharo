Extension { #name : #Character }

{ #category : #'*Collections-Abstract-splitjoin' }
Character >> join: aSequenceableCollection [
	^ self asString join: aSequenceableCollection

]
