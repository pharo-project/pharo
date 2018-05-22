Extension { #name : #Behavior }

{ #category : #'*Ring-Deprecated-Core-Kernel' }
Behavior >> methodNamed: aSelector [

	^ methodDict at: aSelector
]

{ #category : #'*Ring-Deprecated-Core-Kernel' }
Behavior >> methodsInProtocol: aString [

	^ (self organization listAtCategoryNamed: aString) 
			collect: [ :each | (self compiledMethodAt: each) ]
]

{ #category : #'*Ring-Deprecated-Core-Kernel' }
Behavior >> protocols [
	
	^ self organization categories copy
]
