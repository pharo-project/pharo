Class {
	#name : 'ReIsClassDefined',
	#superclass : 'ReClassNameCondition',
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReIsClassDefined >> check [ 

	^ (model classNamed: className) isNotNil
]

{ #category : 'accessing' }
ReIsClassDefined >> violators [ 

	^ { className }
]
