"
I am a command to create new subclass. 
By default I insert the new class between given class and its subclasses (mytargetClass refers to the given class).
If you choose the Add class option, I will add the class as a subclass of the given class and the new class will become a sibling to its subclasses.
"
Class {
	#name : 'SycInsertSubclassCommand',
	#superclass : 'SycNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'testing' }
SycInsertSubclassCommand >> canBeExecuted [

	^ context lastSelectedClass isTrait not
]

{ #category : 'execution' }
SycInsertSubclassCommand >> executeRefactoring [
	"The class name is necessary so the browser can select the new subclass"

	| driver dialog choice dictionnary |
	dictionnary := {
		               (#Insert -> 'Insert your new class in between the target class and its subclasses').
		               (#Add -> 'Add your new class as subclass of the target class and don''t change its hierarchy') } asDictionary.
	dialog := SpSelectDialog new
		          application: StPharoApplication current;
		          title: 'Choose a way to create your new class';
		          message: 'Select the strategy you want to use';
		          items: dictionnary associations ;
		          display: [ :each | each value];
		          onAccept: [ :diag |
				          choice := diag presenter selectedItem key ].
	dialog openModal.
	driver := (ReInsertSubclassDriver superclass: self targetClass packageNames: self packageNames) scopes: refactoringScopes .
	choice = 'Add' ifTrue: [ driver beSibling ].
	driver runRefactoring. 
	newClassName := driver subclass
]

{ #category : 'accessing' }
SycInsertSubclassCommand >> iconName [

	^ #add
]

{ #category : 'accessing' }
SycInsertSubclassCommand >> name [
	^ '(R) Insert subclass'
]

{ #category : 'accessing' }
SycInsertSubclassCommand >> newClassName [

	^ newClassName 
]

{ #category : 'accessing' }
SycInsertSubclassCommand >> newClassName: aString [ 

	newClassName := aString asSymbol
]

{ #category : 'accessing' }
SycInsertSubclassCommand >> order [
	^ 10100.1
]

{ #category : 'accessing' }
SycInsertSubclassCommand >> packageNames [
	"Answer a <Collection> of <Symbol> of the packages available to the new class"
	
	^ PackageOrganizer default packageNames
]

{ #category : 'execution' }
SycInsertSubclassCommand >> prepareFullExecution [
	
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass.

]
