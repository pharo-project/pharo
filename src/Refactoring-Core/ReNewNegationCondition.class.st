Class {
	#name : 'ReNewNegationCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'condition'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReNewNegationCondition class >> on: aRBInstanceVariableHasReferences [ 
	^ self new condition: aRBInstanceVariableHasReferences ; yourself.
]

{ #category : 'accessing - private' }
ReNewNegationCondition >> candidates [ 
	^ condition candidates
]

{ #category : 'checking' }
ReNewNegationCondition >> check [
	" `condition check not` does not work if the condition checks several candidates. "
	^ condition nonViolators isEmpty
]

{ #category : 'accessing' }
ReNewNegationCondition >> condition: aCondition [

	condition := aCondition
]

{ #category : 'displaying' }
ReNewNegationCondition >> errorString [ 

	^ String streamContents: [ :s | self violationMessageOn: s violators: self violators ]
]

{ #category : 'accessing' }
ReNewNegationCondition >> nonViolators [

	^ condition violators
]

{ #category : 'displaying' }
ReNewNegationCondition >> violationMessageOn: aStream violators: theViolators [ 
	condition violationMessageOn: aStream violators: theViolators
]

{ #category : 'accessing' }
ReNewNegationCondition >> violators [

	^ condition nonViolators
]
