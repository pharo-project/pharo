Class {
	#name : 'RBIsClassDefined',
	#superclass : 'RBClassNameCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBIsClassDefined >> check [ 

	^ (model classNamed: className) isNotNil
]

{ #category : 'accessing' }
RBIsClassDefined >> violators [ 

	^ { className }
]
