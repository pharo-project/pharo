Extension { #name : #Behavior }

{ #category : #'*Ring-Definitions-Core' }
Behavior >> methodNamed: aSelector [

	^ self methodDict at: aSelector
]
