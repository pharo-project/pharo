Class {
	#name : 'RBClassesHaveNoSubclassesCondition',
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
RBClassesHaveNoSubclassesCondition >> check [ 

	^ self violators isEmpty
]

{ #category : 'accessing' }
RBClassesHaveNoSubclassesCondition >> classes: aRBClassCollection [

	classes := aRBClassCollection 
]

{ #category : 'testing' }
RBClassesHaveNoSubclassesCondition >> hasSubclasses: aClass excluding: classesList [

	^ ((aClass subclasses collect: [ :each | each name ])
		   copyWithoutAll: classesList) isNotEmpty
]

{ #category : 'printing' }
RBClassesHaveNoSubclassesCondition >> printOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPut: Character cr ]
]

{ #category : 'accessing' }
RBClassesHaveNoSubclassesCondition >> violators [

	^ violators ifNil: [
		  violators := Dictionary new.
		  classes collect: [ :aClass |
			  (self hasSubclasses: aClass excluding: classes) ifTrue: [
				  violators at: aClass put: aClass name , ' has subclasses.' ] ] ]
]
