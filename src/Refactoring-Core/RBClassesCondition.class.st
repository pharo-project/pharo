Class {
	#name : 'RBClassesCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'classes',
		'violators'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBClassesCondition >> check [ 

	^ self violators isEmpty
]

{ #category : 'accessing' }
RBClassesCondition >> classes: aRBClassCollection [

	classes := aRBClassCollection 
]

{ #category : 'checking' }
RBClassesCondition >> isFalse [

	^ self isTrue not
]

{ #category : 'checking' }
RBClassesCondition >> isTrue [ 

	^ self check
]

{ #category : 'accessing' }
RBClassesCondition >> violators [

	^ self subclassResponsibility
]
