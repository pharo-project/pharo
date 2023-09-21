Class {
	#name : 'RBClassesEmptyCondition',
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
RBClassesEmptyCondition >> check [

	^ self violators isEmpty
]

{ #category : 'accessing' }
RBClassesEmptyCondition >> classes: aRBClassCollection [

	classes := aRBClassCollection 
]

{ #category : 'testing' }
RBClassesEmptyCondition >> isEmpty: aClass [

	^ aClass classVariableNames isEmpty and: [
		  aClass instanceVariableNames isEmpty and: [
			  aClass selectors isEmpty ] ]
]

{ #category : 'checking' }
RBClassesEmptyCondition >> isFalse [

	^ self isTrue not
]

{ #category : 'checking' }
RBClassesEmptyCondition >> isTrue [

	^ self check
]

{ #category : 'printing' }
RBClassesEmptyCondition >> printOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPut: Character cr ]
]

{ #category : 'accessing' }
RBClassesEmptyCondition >> violators [

	^ violators ifNil: [
		  violators := Dictionary new.
		  classes do: [ :aClass |
			  (self isEmpty: aClass) ifFalse: [
				  violators at: aClass put: aClass name , ' is not empty.' ] ].
		  violators ]
]
