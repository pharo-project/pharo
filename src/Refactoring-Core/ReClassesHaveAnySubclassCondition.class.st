Class {
	#name : 'ReClassesHaveAnySubclassCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'testing' }
ReClassesHaveAnySubclassCondition >> hasSubclasses: aClass [
	
	^ aClass subclasses isNotEmpty
]

{ #category : 'displaying' }
ReClassesHaveAnySubclassCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' has no subclasses.';
			space ]
]

{ #category : 'accessing' }
ReClassesHaveAnySubclassCondition >> violators [

	^ violators ifNil: [ violators := classes reject: [ :aClass | self hasSubclasses: aClass ] ]
]
