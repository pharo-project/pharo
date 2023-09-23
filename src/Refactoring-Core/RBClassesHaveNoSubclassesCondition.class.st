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

{ #category : 'displaying' }
RBClassesHaveNoSubclassesCondition >> displayStringOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' has subclasses.';
			nextPut: Character cr ]
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

{ #category : 'accessing' }
RBClassesHaveNoSubclassesCondition >> violators [

	^ violators ifNil: [
		  violators := classes select: [ :aClass |
			               self hasSubclasses: aClass excluding: classes ] ]
]
