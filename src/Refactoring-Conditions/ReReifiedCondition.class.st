Class {
	#name : 'ReReifiedCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'subjects'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'accessing' }
ReReifiedCondition >> errorString [

	^ String streamContents: [ :aStream | self violationMessageOn: aStream ]
]

{ #category : 'checking' }
ReReifiedCondition >> isFalse [

	^ self isTrue not
]

{ #category : 'checking' }
ReReifiedCondition >> isTrue [ 

	^ self check
]

{ #category : 'accessing' }
ReReifiedCondition >> nonViolators [

	^ subjects difference: self violators
]

{ #category : 'logical operations' }
ReReifiedCondition >> not [ 

	^ ReNewNegationCondition on: self
]

{ #category : 'accessing - private' }
ReReifiedCondition >> subjects [
	"Complex conditions require this method to compute their own subjects"
	^ subjects
]

{ #category : 'displaying' }
ReReifiedCondition >> violationMessageOn: aStream [

	self subclassResponsibility 
]

{ #category : 'accessing' }
ReReifiedCondition >> violators [

	^ self subclassResponsibility
]
