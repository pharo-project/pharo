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

{ #category : 'checking' }
RBClassesHaveNoSubclassesCondition >> isFalse [

	^ self isTrue not
]

{ #category : 'checking' }
RBClassesHaveNoSubclassesCondition >> isTrue [ 

	^ self check
]

{ #category : 'printing' }
RBClassesHaveNoSubclassesCondition >> printOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' has subclasses.';
			nextPut: Character cr ]
]

{ #category : 'accessing' }
RBClassesHaveNoSubclassesCondition >> violators [

	^ violators ifNil: [
		  violators := OrderedCollection new.
		  classes collect: [ :aClass |
			  (self hasSubclasses: aClass excluding: classes) ifTrue: [
				  violators add: aClass ] ].
		  violators ]
]
