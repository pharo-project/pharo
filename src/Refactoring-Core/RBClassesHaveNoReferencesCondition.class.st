"
I am a precondition that checks if a group of classes has references.

You can call check to see if classes have references.
Then you can use `violators` to get a dictionary mapping each class to its references.
"
Class {
	#name : 'RBClassesHaveNoReferencesCondition',
	#superclass : 'RBClassesCondition',
	#instVars : [
		'model',
		'referencingClassesDictionary'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
RBClassesHaveNoReferencesCondition >> model: aRBNamespace [

	model := aRBNamespace
]

{ #category : 'displaying' }
RBClassesHaveNoReferencesCondition >> violationMessageOn: aStream [

	(self violators reject: [ :refs | refs isEmpty ])
		keysAndValuesDo: [ :class :refs |
			aStream
				nextPutAll: class name;
				nextPutAll: ' has ';
				nextPutAll: refs size asString;
				nextPutAll: ' references.';
				space ]
]

{ #category : 'accessing' }
RBClassesHaveNoReferencesCondition >> violators [

	^ referencingClassesDictionary ifNil: [
		  referencingClassesDictionary := Dictionary new.

		  classes do: [ :aClass |
			  | methodsReferencingClass |
			  methodsReferencingClass := (model methodsReferencingClass: aClass) reject: [ :each |
						    classes includes: each methodClass ].
			  methodsReferencingClass ifNotEmpty: [ 
				  referencingClassesDictionary
					  at: aClass
					  put: methodsReferencingClass ] ].
			referencingClassesDictionary 
		]
]
