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

	| refactorings model |
	model := ( RBClassModelFactory rbNamespace onEnvironment: RBBrowserEnvironment new )
				name: self printString;
				yourself.
	refactorings := classes flatCollect: [:eachClass |
		eachClass	 slots collect: [:var | 
				RBAbstractInstanceVariableRefactoring
					model: model variable: var name class:eachClass ]].
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

{ #category : #execution }
SycAbstractAllInstVarAccessorsCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SycAbstractAllInstVarAccessorsCommand >> isComplexRefactoring [
	^false
]

{ #category : #testing }
SycAbstractAllInstVarAccessorsCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	classes := classes collect: [:each | aToolContext currentMetaLevelOf: each]
]
