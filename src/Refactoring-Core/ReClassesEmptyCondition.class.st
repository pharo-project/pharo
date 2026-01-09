"
I check that classes have no instance variables, no shared variables and no selectors
"
Class {
	#name : 'ReClassesEmptyCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'testing' }
ReClassesEmptyCondition >> isEmpty: aClass [

	^ aClass classVariableNames isEmpty and: [
		  aClass instanceVariableNames isEmpty and: [
			  aClass selectors isEmpty ] ]
]

{ #category : 'displaying' }
ReClassesEmptyCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' is not empty.';
			space ]
]

{ #category : 'displaying' }
ReClassesEmptyCondition >> violationMessageOn: aStream violators: theViolators [
	| isNegated |
	self flag: 'I am the template to improve other implementations!'.
	theViolators ifEmpty:[^self].
	isNegated := self violators ~= theViolators.
	
	theViolators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: '<1? is not:is>';
			nextPutAll: ' empty.';
			space ]
]

{ #category : 'accessing' }
ReClassesEmptyCondition >> violators [

	^ violators ifNil: [
		  violators := classes reject: [ :aClass | self isEmpty: aClass ] ]
]
