Class {
	#name : 'RBClassesAreNotMetaClassCondition',
	#superclass : 'RBClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'displaying' }
RBClassesAreNotMetaClassCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' is a meta class.';
			space ].
	aStream nextPutAll: 'Cannot remove just the metaclass.'
]

{ #category : 'accessing' }
RBClassesAreNotMetaClassCondition >> violators [

	^ violators ifNil: [
		  violators := classes select: [ :class | class isMeta ] ]
]
