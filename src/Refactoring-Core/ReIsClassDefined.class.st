Class {
	#name : 'ReIsClassDefined',
	#superclass : 'ReClassNameCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReIsClassDefined >> check [ 

	^ (model classNamed: className) isNotNil
]

{ #category : 'accessing' }
ReIsClassDefined >> violators [ 

	^ { className }
]
