Class {
	#name : 'RBClassesEmptyCondition',
	#superclass : 'RBClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'displaying' }
RBClassesEmptyCondition >> displayStringOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' is not empty.';
			nextPut: Character cr ]
]

{ #category : 'testing' }
RBClassesEmptyCondition >> isEmpty: aClass [

	^ aClass classVariableNames isEmpty and: [
		  aClass instanceVariableNames isEmpty and: [
			  aClass selectors isEmpty ] ]
]

{ #category : 'accessing' }
RBClassesEmptyCondition >> violators [

	^ violators ifNil: [
		  violators := classes reject: [ :aClass | self isEmpty: aClass ] ]
]
