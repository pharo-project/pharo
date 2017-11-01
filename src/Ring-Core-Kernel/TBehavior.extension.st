Extension { #name : #TBehavior }

{ #category : #'*Ring-Core-Kernel' }
TBehavior >> methodNamed: aSelector [

	^ self methodDict at: aSelector
]

{ #category : #'*Ring-Core-Kernel' }
TBehavior >> methodsInProtocol: aString [

	^ (self organization listAtCategoryNamed: aString) 
			collect: [ :each | (self compiledMethodAt: each) ]
]

{ #category : #'*Ring-Core-Kernel' }
TBehavior >> protocols [
	
	^ self organization categories copy
]
