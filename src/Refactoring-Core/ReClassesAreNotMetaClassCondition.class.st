Class {
	#name : 'ReClassesAreNotMetaClassCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'displaying' }
ReClassesAreNotMetaClassCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' is a meta class.';
			space ].
	aStream nextPutAll: 'Cannot remove just the metaclass.'
]

{ #category : 'accessing' }
ReClassesAreNotMetaClassCondition >> violators [

	^ violators ifNil: [
		  violators := classes select: [ :class | class isMeta ] ]
]
