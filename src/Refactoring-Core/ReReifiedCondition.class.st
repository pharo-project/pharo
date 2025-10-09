Class {
	#name : 'ReReifiedCondition',
	#superclass : 'RBCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
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

	^ self subjects difference: self violators
]

{ #category : 'logical operations' }
ReReifiedCondition >> not [ 

	^ ReNewNegationCondition on: self
]

{ #category : 'accessing' }
ReReifiedCondition >> subjects [
	
	self subclassResponsibility 
]

{ #category : 'displaying' }
ReReifiedCondition >> violationMessageOn: aStream [

	self subclassResponsibility 
]

{ #category : 'accessing' }
ReReifiedCondition >> violators [

	^ self subclassResponsibility
]
