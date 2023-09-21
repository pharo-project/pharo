Class {
	#name : 'RBClassesHaveNoSubclassesCondition',
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
RBClassesHaveNoSubclassesCondition >> check [ 

	violators := Dictionary new.
	classes do: [ :aClass | 
		(self hasSubclasses: aClass excluding: classes)
			ifTrue: [ violators at: aClass  put: aClass name, 'has subclasses.'  ]
	].
	^ violators isEmpty
]

{ #category : 'accessing' }
RBClassesHaveNoSubclassesCondition >> classes: aRBClassCollection [

	classes := aRBClassCollection 
]

{ #category : 'testing' }
RBClassesHaveNoSubclassesCondition >> hasSubclasses: aClass excluding: classesList [

	^ ((aClass subclasses collect: [ :each | each name ])
		   copyWithoutAll: classesList) isNotEmpty
]

{ #category : 'accessing' }
RBClassesHaveNoSubclassesCondition >> violators [ 
	^ violators
]
