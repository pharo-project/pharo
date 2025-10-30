Class {
	#name : 'ReClassesAreAbstractCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'displaying' }
ReClassesAreAbstractCondition >> violationMessageOn: aStream violators: theViolators [
	| verb |
	theViolators = self violators 
		ifTrue: [ verb := 'is not' ] 
		ifFalse: [ self assert: theViolators = self nonViolators. verb := 'is' ].
	theViolators ifEmpty: [ ^ self ] ifNotEmpty: [
			aStream nextPutAll: (', ' join: (theViolators collect: #name)).
			aStream nextPutAll: ' '.
			aStream nextPutAll: verb.
			aStream nextPutAll: ' an abstract class.' ]
]

{ #category : 'accessing' }
ReClassesAreAbstractCondition >> violators [

	^ violators ifNil: [
		  violators := classes reject: [ :aClass | aClass isAbstract ] ]
]
