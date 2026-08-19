"
I'm a condition that checks if any of given instance variables is referenced in given class
"
Class {
	#name : 'ReReferencesInstanceVariableCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'instanceVariables'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'instance creation' }
ReReferencesInstanceVariableCondition class >> class: aRBAbstractClass inModel: aRBNamespace instanceVariables: aCollection [

	^ self new
		  class: aRBAbstractClass inModel: aRBNamespace instanceVariables: aCollection;
		  yourself
]

{ #category : 'checking' }
ReReferencesInstanceVariableCondition >> check [

	violators := instanceVariables flatCollect: [ :v | class whichSelectorsReferToInstanceVariable: v ].

	^ violators isEmpty
]

{ #category : 'instance creation' }
ReReferencesInstanceVariableCondition >> class: aRBAbstractClass inModel: aRBNamespace instanceVariables: aCollection [

	class := aRBAbstractClass.
	model := aRBNamespace.
	instanceVariables := aCollection
]

{ #category : 'displaying' }
ReReferencesInstanceVariableCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
			  aStream
				  nextPutAll: ('The selector {1} references an instance variable in the class {2}' format: {
								   violator.
								   class name });
				  nextPut: Character cr ]
]
