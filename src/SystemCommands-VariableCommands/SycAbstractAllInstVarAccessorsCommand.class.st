"
I command to launch the Abstract refactorings: create accessors and abstract the direct instance variable uses into accessors. 
"
Class {
	#name : #SycAbstractAllInstVarAccessorsCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'variable'
	],
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #activation }
SycAbstractAllInstVarAccessorsCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation  by: $a meta, $a meta for: ClyClass asCalypsoItemContext 
]

{ #category : #activation }
SycAbstractAllInstVarAccessorsCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyVariableContextOfFullBrowser 
]

{ #category : #activation }
SycAbstractAllInstVarAccessorsCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemFor: ClySourceCodeContext
]

{ #category : #accessing }
SycAbstractAllInstVarAccessorsCommand >> asRefactorings [
	^ {RBAbstractInstanceVariableRefactoring 
		variable: variable actualVariable name
		class: variable definingClass}
]

{ #category : #accessing }
SycAbstractAllInstVarAccessorsCommand >> defaultMenuIconName [ 
	^ #smallAdd
]

{ #category : #accessing }
SycAbstractAllInstVarAccessorsCommand >> defaultMenuItemName [
	^'Abstract instance variables'
]

{ #category : #execution }
SycAbstractAllInstVarAccessorsCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SycAbstractAllInstVarAccessorsCommand >> isComplexRefactoring [
	^ true
]

{ #category : #testing }
SycAbstractAllInstVarAccessorsCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	variable := aToolContext lastSelectedVariable.
]
