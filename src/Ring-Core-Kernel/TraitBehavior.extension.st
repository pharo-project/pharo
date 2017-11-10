Extension { #name : #TraitBehavior }

{ #category : #'*Ring-Core-Kernel' }
TraitBehavior >> methodNamed: aSelector [

	^ self methodDict at: aSelector
]

{ #category : #'*Ring-Core-Kernel' }
TraitBehavior >> methodsInProtocol: aString [

	^ (self organization listAtCategoryNamed: aString) 
			collect: [ :each | (self compiledMethodAt: each) ]
]

{ #category : #'*Ring-Core-Kernel' }
TraitBehavior >> protocols [
	
	^ self organization categories copy
]
