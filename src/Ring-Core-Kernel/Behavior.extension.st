Extension { #name : #Behavior }

{ #category : #'*Ring-Core-Kernel' }
Behavior >> methodNamed: aSelector [

	^ self methodDict at: aSelector
]

{ #category : #'*Ring-Core-Kernel' }
Behavior >> methodsInProtocol: aString [

	^ (self organization listAtCategoryNamed: aString) 
			collect: [ :each | (self compiledMethodAt: each) ]
]

{ #category : #'*Ring-Core-Kernel' }
Behavior >> protocols [
	
	^ self organization categories copy
]
