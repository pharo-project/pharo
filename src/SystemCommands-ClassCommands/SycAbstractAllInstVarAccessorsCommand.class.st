"
I command to launch the Abstract refactorings: create accessors and abstract the direct instance variable uses into accessors. 
"
Class {
	#name : #SycAbstractAllInstVarAccessorsCommand,
	#superclass : #SycClassCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #activation }
SycAbstractAllInstVarAccessorsCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation  by: $a meta, $a meta for: ClyClass asCalypsoItemContext 
]

{ #category : #activation }
SycAbstractAllInstVarAccessorsCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2.1 for: ClyFullBrowserClassContext 
]

{ #category : #accessing }
SycAbstractAllInstVarAccessorsCommand >> asRefactorings [
	| refactorings env |
	refactorings := classes flatCollect: [:each |
		each value collect: [:var | | refactoring |
			env ifNil: [ refactoring := var createRefactoring: RBAbstractInstanceVariableRefactoring for: each key.
				env := refactoring model ]
				ifNotNil: [refactoring := var createRefactoring: RBAbstractInstanceVariableRefactoring for: each key in: env ].
			refactoring ]].
	^ refactorings
]

{ #category : #accessing }
SycAbstractAllInstVarAccessorsCommand >> defaultMenuIconName [ 
	^ #smallAdd
]

{ #category : #accessing }
SycAbstractAllInstVarAccessorsCommand >> defaultMenuItemName [
	^'Abstract instance variables'
]

{ #category : #testing }
SycAbstractAllInstVarAccessorsCommand >> isComplexRefactoring [ 
	^ true
]

{ #category : #testing }
SycAbstractAllInstVarAccessorsCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	classes := classes collect: [:each | aToolContext currentMetaLevelOf: each].
	self selectVariables.
]

{ #category : #execution }
SycAbstractAllInstVarAccessorsCommand >> selectVariables [ 
	classes := classes collect: [:aClass | | vars dialog |
		vars := aClass slots asOrderedCollection.
		dialog := RBVariablesSelectionPresenter 
		   label: aClass name,' - Select all variables to convert abstract'
			withItems: vars 
			selecting: vars. 
		dialog cancelled ifTrue: [ CmdCommandAborted signal ].
		aClass -> dialog presenter selectedItems
		]
]
