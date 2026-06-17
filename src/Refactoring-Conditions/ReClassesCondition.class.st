Class {
	#name : 'ReClassesCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'classes',
		'violators'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReClassesCondition >> check [ 

	^ self violators isEmpty
]

{ #category : 'accessing' }
ReClassesCondition >> classes: aRBClassCollection [

	classes := aRBClassCollection.
	subjects := classes.
]
