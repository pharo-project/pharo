Class {
	#name : 'ReBlockCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'violators',
		'block'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReBlockCondition >> block: aBlock [

	block := aBlock
]

{ #category : 'checking' }
ReBlockCondition >> check [ 

	^ block value

]

{ #category : 'checking' }
ReBlockCondition >> violatorErrorString: aString [

	violators := aString
]
