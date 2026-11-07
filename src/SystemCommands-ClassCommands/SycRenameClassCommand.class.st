"
Rename Class Refactoring: Change the name of a selected class, update all references, and preview changes before applying.

"
Class {
	#name : 'SycRenameClassCommand',
	#superclass : 'SycSingleClassCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycRenameClassCommand >> executeRefactoring [

	toolContext isFullBrowserClassContext
		ifTrue: [
			(ReRenameClassDriver rename: targetClass name)
				model: RBBrowserEnvironment new;
				scopes: toolContext refactoringScopes;
				runRefactoring ]
		ifFalse: [
			toolContext isClassDefinitionContext
				ifTrue: [
					(ReRenameClassDriver rename: toolContext selectedSourceNode value)
						model: RBBrowserEnvironment new;
						scopes: toolContext refactoringScopes;
						runRefactoring ]
				ifFalse: [ "we are in selection in a method "
					(ReRenameClassDriver rename: toolContext selectedSourceNode value name)
						model: RBBrowserEnvironment new;
						scopes: toolContext refactoringScopes;
						runRefactoring ] ]
]

{ #category : 'accessing' }
SycRenameClassCommand >> iconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameClassCommand >> name [
	^'(Old refactor) rename class'
]

{ #category : 'accessing' }
SycRenameClassCommand >> order [

	^ 1.2
]

{ #category : 'preparation' }
SycRenameClassCommand >> prepareFullExecution [

	super prepareFullExecution.
	toolContext := self context
]
