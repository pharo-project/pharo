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
		  violators := ((self theClass allSubclasses collect: [ :each |
			                 each name ]) includesAll: subclasses)
			               ifTrue: [ {  } ]
			               ifFalse: [ { self class } ] ]
]
