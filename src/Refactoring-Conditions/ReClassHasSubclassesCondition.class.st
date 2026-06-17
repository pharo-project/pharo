Class {
	#name : 'ReClassHasSubclassesCondition',
	#superclass : 'ReClassesCondition',
	#instVars : [
		'subclasses'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'accessing' }
ReClassHasSubclassesCondition >> class: aClass [

	classes := { aClass }.
	subjects := classes
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
			               ifFalse: [ subjects ] ]
]
