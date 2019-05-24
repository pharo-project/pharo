Class {
	#name : #RBDeprecateClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBDeprecateClassTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBDeprecateClassTransformation 
							class: self changeMock name)
							transform. 

	self assert: transformation model changes changes size equals: 4.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: class comment equals: 'Deprecated!!! Use superclass'.
	
	class := transformation model metaclassNamed: self changeMock name.
	
	self assert: (class parseTreeFor: #new)
		equals: (self parseMethod: 'new
				self deprecated: ''Use superclass '' on: ''4 May 2016''  in: #Pharo60.
				^ super new').
	self assert: (class parseTreeFor: #deprecated)
		equals: (self parseMethod: 'deprecated ^ true').
	self assert: (class parseTreeFor: #systemIcon)
		equals: (self parseMethod: 'systemIcon
				^ Smalltalk ui icons iconNamed: #packageDelete').
]
