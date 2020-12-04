Extension { #name : #Behavior }

{ #category : #'*Ring-Definitions-Core' }
Behavior >> methodNamed: aSelector [

	^ self methodDict at: aSelector
]

{ #category : #'*Ring-Definitions-Core' }
Behavior >> protocols [
	
	^ self organization categories copy
]
