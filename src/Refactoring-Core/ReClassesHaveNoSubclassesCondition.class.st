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
	self deprecated: 'Use violationMessageOn:violators: instead'.
	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' has subclasses.';
			space ]
]

{ #category : 'displaying' }
ReClassesHaveNoSubclassesCondition >> violationMessageOn: aStream violators: theViolators [ 
	| verb |
	theViolators ifEmpty: [ ^self ].
	verb := theViolators = self violators ifTrue: [ ' does not have' ] ifFalse: [ ' has' ].
	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: verb;
			nextPutAll: ' subclasses.';
			space ]
]

{ #category : 'accessing' }
ReClassesHaveNoSubclassesCondition >> violators [

	^ violators ifNil: [
		  violators := classes select: [ :aClass |
			               self hasSubclasses: aClass excluding: classes ] ]
]
