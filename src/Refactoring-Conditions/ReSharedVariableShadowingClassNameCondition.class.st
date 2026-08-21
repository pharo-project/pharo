"
I am a condition that tests whether a class rename will result in shadowing of the class reference by an existing shared variable that has the same name as the new class name.
"
Class {
	#name : 'ReSharedVariableShadowingClassNameCondition',
	#superclass : 'ReVariableNameCondition',
	#instVars : [
		'model',
		'oldName'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'instance creation' }
ReSharedVariableShadowingClassNameCondition class >> name: aName oldName: oldName model: aModel [

	^ (self name: aName)
		oldName: oldName
		model: aModel;
		yourself
]

{ #category : 'checking' }
ReSharedVariableShadowingClassNameCondition >> check [

	^ self violator isNil or: [ self violator isEmpty ]
]

{ #category : 'accessing' }
ReSharedVariableShadowingClassNameCondition >> oldName: oldClassNameString model: aModel [

	oldName := oldClassNameString.
	model := aModel
]

{ #category : 'displaying' }
ReSharedVariableShadowingClassNameCondition >> violationMessageOn: aStream [

	aStream nextPutAll: 'There is existing shared variable with name:';
		nextPutAll: name;
		cr;
		nextPutAll: 'Renaming the class to this name will change existing symbol lookup,';
		cr;
		nextPutAll: 'since following classes and their methods reference the renaming class.';
		cr.
	self violator do: [ :v | aStream nextPutAll: v asString; cr. ].

]

{ #category : 'accessing' }
ReSharedVariableShadowingClassNameCondition >> violator [
	^ violator ifNil: [ 
		"We can't rename a class to a name of existing shared variable if
		the class that has variable defined is also referenceing class that is being renamed.
		We then change symbol lookup, and old class will resolve to shared var."
		| rbClass |
		rbClass := model classNamed: oldName.
		violator := OrderedCollection new.
		model allReferencesToClass: rbClass do: [ :method |
			(method modelClass hierarchyDefinesVariable: name) ifTrue: [ 
					violator add: method ] ].
		violator
		 ]
]
