Class {
	#name : 'ReClassesHaveNoSubclassesCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'testing' }
ReClassesHaveNoSubclassesCondition >> hasSubclasses: aClass excluding: classesList [

	^ ((aClass subclasses collect: [ :each | each name ])
		   copyWithoutAll: classesList) isNotEmpty
]

{ #category : 'displaying' }
ReClassesHaveNoSubclassesCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' has subclasses.';
			space ]
]

{ #category : 'accessing' }
ReClassesHaveNoSubclassesCondition >> violators [

	^ violators ifNil: [
		  violators := classes select: [ :aClass |
			               self hasSubclasses: aClass excluding: classes ] ]
]
