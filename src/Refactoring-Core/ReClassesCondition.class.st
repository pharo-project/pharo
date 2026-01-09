Class {
	#name : 'ReClassesCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'classes',
		'violators'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing - private' }
ReClassesCondition >> candidates [
	^ classes
]

{ #category : 'accessing' }
ReClassesCondition >> classes: aRBClassCollection [

	classes := aRBClassCollection
]
