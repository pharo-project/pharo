Class {
	#name : 'ReClassesAreNotMetaClassCondition',
	#superclass : 'ReClassesCondition',
	#instVars : [
		'message'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'initialization' }
ReClassesAreNotMetaClassCondition >> initialize [ 

	super initialize.
	message := ''
]

{ #category : 'accessing' }
ReClassesAreNotMetaClassCondition >> message: aStringExplaining [

	message := aStringExplaining 
]

{ #category : 'displaying' }
ReClassesAreNotMetaClassCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator name;
			nextPutAll: ' is a meta class.' ].
	message ifNotEmpty: [ aStream space ].
	aStream nextPutAll: message
]

{ #category : 'accessing' }
ReClassesAreNotMetaClassCondition >> violators [

	^ violators ifNil: [
		  violators := subjects select: [ :class | class isMeta ] ]
]
