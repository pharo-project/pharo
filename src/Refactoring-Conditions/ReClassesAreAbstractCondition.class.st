Class {
	#name : 'ReClassesAreAbstractCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'displaying' }
ReClassesAreAbstractCondition >> violationMessageOn: aStream [

	self violators ifEmpty: [ ^ self ].
	aStream nextPutAll: (', ' join: (self violators collect: #name)).
	aStream nextPutAll: ' is not an abstract class.'
]

{ #category : 'accessing' }
ReClassesAreAbstractCondition >> violators [

	^ violators ifNil: [
		  violators := classes reject: [ :aClass | aClass isAbstract ] ]
]
