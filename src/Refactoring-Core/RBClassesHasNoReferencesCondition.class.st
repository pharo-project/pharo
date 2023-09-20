"
I am a precondition that checks if a group of classes has references.

You can call check to see if classes have references.
Then you can use `violators` to get a dictionary mapping each class to its references.
"
Class {
	#name : 'RBClassesHasNoReferencesCondition',
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
RBClassesHasNoReferencesCondition >> check [

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
RBClassesHasNoReferencesCondition >> classes: aRBClassCollection [

	classes := aRBClassCollection 
]

{ #category : 'accessing' }
RBClassesHasNoReferencesCondition >> model: aRBNamespace [

	model := aRBNamespace
]

{ #category : 'accessing' }
RBClassesHasNoReferencesCondition >> violators [

	^ referencingClassesDictionary 
]
