Class {
	#name : 'ReReifiedCondition',
	#superclass : 'RBCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing - private' }
ReReifiedCondition >> candidates [
	"Candidates are all objects evaluated against the defined conditions. Any object that fails to meet those conditions is considered a violator."
	self subclassResponsibility 
]

{ #category : 'checking' }
ReReifiedCondition >> check [ 

	^ self violators isEmpty
]

{ #category : 'accessing' }
ReReifiedCondition >> errorString [

	^ String streamContents: [ :s | self violationMessageOn: s violators: self violators ]
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

	^ self candidates difference: self violators
]

{ #category : 'logical operations' }
ReReifiedCondition >> not [ 

	^ ReNewNegationCondition on: self
]

{ #category : 'displaying' }
ReReifiedCondition >> violationMessageOn: aStream violators: theViolators [
	| isNegated |
	theViolators ifEmpty:[^self].
	isNegated := self violators ~= theViolators.
	
	theViolators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ('<1? is not:is>' expandMacrosWith: isNegated);
			nextPutAll: ' empty.';
			space ]
]

{ #category : 'accessing' }
ReReifiedCondition >> violators [

	^ self subclassResponsibility
]
