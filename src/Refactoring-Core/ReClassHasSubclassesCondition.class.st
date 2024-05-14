"
I am a refactoring precondition to check that a a collection of specific subclasses are in the hierarchy of subclasses of a class
"
Class {
	#name : 'ReClassHasSubclassesCondition',
	#superclass : 'ReClassesCondition',
	#instVars : [
		'subclasses'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReClassHasSubclassesCondition >> class: aClass [

	classes := { aClass }
]

{ #category : 'private' }
ReClassHasSubclassesCondition >> class: aClass hasSubclasses: subclassesList [

	^ (aClass allSubclasses collect: [ :each | each name ]) includesAll:
		  subclassesList
]

{ #category : 'accessing' }
ReClassHasSubclassesCondition >> subclassesList: aCollection [

	subclasses := aCollection
]

{ #category : 'class membership' }
ReClassHasSubclassesCondition >> theClass [

	classes ifEmpty: [ ^ nil ].
	^ classes first
]

{ #category : 'accessing' }
ReClassHasSubclassesCondition >> violators [

	^ violators ifNil: [
		  violators := (self class: self theClass hasSubclasses: subclasses)
			               ifTrue: [ { } ]
			               ifFalse: [ { self class } ] ]
]
