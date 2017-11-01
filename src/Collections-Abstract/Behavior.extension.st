Extension { #name : #Behavior }

{ #category : #'*Collections-Abstract' }
Behavior class >> sortBlock [
	^ [ :a :b | a name <= b name ]
]
