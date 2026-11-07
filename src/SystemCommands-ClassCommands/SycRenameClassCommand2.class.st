"
Rename Class Refactoring: Change the name of a selected class, update all references, and preview changes before applying.

"
Class {
	#name : 'SycRenameClassCommand2',
	#superclass : 'SycSingleClassCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycRenameClassCommand2 >> executeRefactoring [

	toolContext isFullBrowserClassContext
		ifTrue: [
			(ReRenameClassDriver2 rename: targetClass name)
				model: RBBrowserEnvironment new;
				scopes: toolContext refactoringScopes;
				runRefactoring ]
		ifFalse: [
			toolContext isClassDefinitionContext
				ifTrue: [
					(ReRenameClassDriver2 rename: toolContext selectedSourceNode value)
						model: RBBrowserEnvironment new;
						scopes: toolContext refactoringScopes;
						runRefactoring ]
				ifFalse: [ "we are in selection in a method "
					(ReRenameClassDriver2 rename: toolContext selectedSourceNode value name)
						model: RBBrowserEnvironment new;
						scopes: toolContext refactoringScopes;
						runRefactoring ] ]
]

{ #category : 'accessing' }
SycRenameClassCommand2 >> iconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameClassCommand2 >> name [
	^'(R) Rename class'
]

{ #category : 'accessing' }
SycRenameClassCommand2 >> order [

	^ 1.2
]

{ #category : 'preparation' }
SycRenameClassCommand2 >> prepareFullExecution [

	super prepareFullExecution.
	toolContext := self context
]
