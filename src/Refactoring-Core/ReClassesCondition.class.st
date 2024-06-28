Class {
	#name : 'ReClassesCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'classes',
		'violators'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReClassesCondition >> check [ 

	^ self violators isEmpty
]

{ #category : 'accessing' }
ReClassesCondition >> classes: aRBClassCollection [

	classes := aRBClassCollection 
]

{ #category : 'checking' }
ReClassesCondition >> isFalse [

	^ self isTrue not
]

{ #category : 'checking' }
ReClassesCondition >> isTrue [ 

	^ self check
]
