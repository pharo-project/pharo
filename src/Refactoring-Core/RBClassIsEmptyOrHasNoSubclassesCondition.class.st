Class {
	#name : 'RBClassIsEmptyOrHasNoSubclassesCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'classes',
		'violators'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBClassIsEmptyOrHasNoSubclassesCondition >> check [ 

	violators := Dictionary new.
	classes do: [ :aClass | 
		(self isEmpty: aClass)  
			ifFalse: [ violators at: aClass put: aClass name, ' is not empty.' ].
		(self hasSubclasses: aClass excluding: classes)
			ifFalse: [ violators at: aClass 
								ifPresent: [ :p | violators at: aClass put: aClass name, 'has subclasses and ', p  ]
								ifAbsent: [ violators at: aClass put: aClass name, 'has subclasses.'  ]
	]].
	^ violators isEmpty
]

{ #category : 'testing' }
RBClassIsEmptyOrHasNoSubclassesCondition >> hasSubclasses: aClass excluding: classesList [

	^ ((aClass subclasses collect: [ :each | each name ])
		   copyWithoutAll: classesList) isNotEmpty
]

{ #category : 'testing' }
RBClassIsEmptyOrHasNoSubclassesCondition >> isEmpty: aClass [

	aClass classVariableNames isEmpty and: [
		aClass instanceVariableNames isEmpty and: [
			aClass selectors isEmpty ] ]
]

{ #category : 'accessing' }
RBClassIsEmptyOrHasNoSubclassesCondition >> violators [ 
	^ violators
]
