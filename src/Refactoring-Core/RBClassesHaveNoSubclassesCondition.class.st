Class {
	#name : 'RBClassesHaveNoSubclassesCondition',
	#superclass : 'RBClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'testing' }
RBClassesHaveNoSubclassesCondition >> hasSubclasses: aClass excluding: classesList [

	^ ((aClass subclasses collect: [ :each | each name ])
		   copyWithoutAll: classesList) isNotEmpty
]

{ #category : 'displaying' }
RBClassesHaveNoSubclassesCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' has subclasses.';
			space ]
]

{ #category : 'accessing' }
RBClassesHaveNoSubclassesCondition >> violators [

	^ violators ifNil: [
		  violators := classes select: [ :aClass |
			               self hasSubclasses: aClass excluding: classes ] ]
]
