Class {
	#name : 'RePackageNameCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'packageName'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
RePackageNameCondition >> candidates [
	^ { packageName }
]

{ #category : 'accessing' }
RePackageNameCondition >> packageNamed: aName [
	packageName := aName 
]
