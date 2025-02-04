"
I am a driver responsible for creating and invoking RenameAndDeprecateClassTransformation.

My responsiblities include:
- Requesting new class name from the user
- Configuring refactoring with the new name
- Executing refactoring
- Displaying changes to the user

You can invoke me with:
```smalltalk
(RBRenameAndDeprecateClassDriver
	className: 'RBLintRuleTestData'
	scopes: { RBNamespace new environment }) runRefactoring 
```
"
Class {
	#name : 'RBDeprecateClassDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'className',
		'newClassName'
	],
	#category : 'Refactoring-UI-Drivers to migrate to new architecture',
	#package : 'Refactoring-UI',
	#tag : 'Drivers to migrate to new architecture'
}

{ #category : 'instance creation' }
RBDeprecateClassDriver class >> className: aClassName scopes: refactoringScopes [

	^ self new className: aClassName scopes: refactoringScopes
]

{ #category : 'initialization' }
RBDeprecateClassDriver >> className: aClassName scopes: refactoringScopes [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	className := aClassName
]

{ #category : 'execution' }
RBDeprecateClassDriver >> deprecateAndMigrateReferences [

	newClassName := self requestClassToMigrateReferencesTo.
	refactoring := RBDeprecateClassRefactoring deprecate: className in: newClassName.
	refactoring generateChanges.
	
	self openPreviewWithChanges: refactoring changes.
	
]

{ #category : 'execution' }
RBDeprecateClassDriver >> deprecateClass [

	refactoring := RBDeprecateClassTransformation model: model className: className.
	refactoring generateChanges.
	
	self openPreviewWithChanges: refactoring changes.
]

{ #category : 'execution' }
RBDeprecateClassDriver >> renameAndDeprecate [

	refactoring := RBRenameAndDeprecateClassTransformation
		               model: model
		               rename: className.
	newClassName := self requestNewClassName.
	refactoring newClassName: newClassName.
	"Check if the new name is OK..."
	refactoring generateChanges.
	
	self openPreviewWithChanges: refactoring changes.
	
]

{ #category : 'ui - requests' }
RBDeprecateClassDriver >> requestClassToMigrateReferencesTo [

	newClassName := SpSelectDialog new
		                title: 'Class to migrate ' , className;
		                label: 'The new class that will be used to migrate all references of the old class to it.';
		                items: self class environment allClasses; "This should probably check in the environment of the class to deprecate but I don't know how to get it here"
		                display: [ :each | each name ];
		                displayIcon: [ :each | self iconNamed: each systemIconName ];
		                openModal.
	^ newClassName name
]

{ #category : 'ui - requests' }
RBDeprecateClassDriver >> requestNewClassName [

	newClassName := SpRequestDialog new
		                title: 'Rename and deprecate class';
		                label: 'New name of the class';
		                text: className;
		                acceptLabel: 'OK';
		                cancelLabel: 'Cancel';
		                openModal.

	^ newClassName
]

{ #category : 'execution' }
RBDeprecateClassDriver >> runRefactoring [

	| select |
	select := self selectDialog.
	
	[
		[ select ifNotNil: [ select action ] ]
		on: RBApplicabilityChecksFailedError do: [ :err | RBRefactoringError signal: err errorString ]
	]
	on: RBBreakingChangeChecksFailedWarning do: [ :err | RBRefactoringWarning signal: err errorString. err resume ]
]

{ #category : 'execution' }
RBDeprecateClassDriver >> selectDialog [ 

	^ SpSelectDialog new 
		title: 'Deprecate class with strategy';
		label: 'Select a strategy';
		items: (ReDeprecateClassChoice subclasses collect: [ :each | each  new driver: self]);
		display: [ :each | each description ];
		displayIcon: [ :each | self iconNamed: each systemIconName ];
		openModal.

]
