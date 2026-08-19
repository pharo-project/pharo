"
I check that classes have no instance variables, no shared variables and no selectors
"
Class {
	#name : 'ReClassesEmptyCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
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

{ #category : 'accessing' }
ReClassesEmptyCondition >> violators [

	^ violators ifNil: [
		  violators := classes reject: [ :aClass | self isEmpty: aClass ] ]
]
