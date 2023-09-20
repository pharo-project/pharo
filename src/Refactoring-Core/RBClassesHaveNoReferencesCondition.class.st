"
I am a precondition that checks if a group of classes has references.

You can call check to see if classes have references.
Then you can use `violators` to get a dictionary mapping each class to its references.
"
Class {
	#name : 'RBClassesHaveNoReferencesCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'model',
		'classes',
		'referencingClassesDictionary'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBClassesHaveNoReferencesCondition >> check [

	referencingClassesDictionary := Dictionary new.

	classes do: [ :aClass |
		referencingClassesDictionary
			at: aClass
			put:
				(((model classesReferencingClass: aClass) collect: [ :each | each ])
					 copyWithoutAll: classes) ].
	^ referencingClassesDictionary allSatisfy: [ :val | val isEmpty ]
]

{ #category : 'accessing' }
RBClassesHaveNoReferencesCondition >> classes: aRBClassCollection [

	classes := aRBClassCollection 
]

{ #category : 'accessing' }
RBClassesHaveNoReferencesCondition >> model: aRBNamespace [

	model := aRBNamespace
]

{ #category : 'accessing' }
RBClassesHaveNoReferencesCondition >> violators [

	^ referencingClassesDictionary 
]
