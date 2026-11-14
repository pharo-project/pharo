"
I am a command to invert hierarhcy of variable list in full browser.
By default variables are ordered from superclass to subclass. And I allow to toggle this order
"
Class {
	#name : 'ClyInvertVariableOrderCommand',
	#superclass : 'ClyCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-MethodGroups',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-MethodGroups'
}

{ #category : 'testing' }
ClyInvertVariableOrderCommand >> canBeExecuted [

	^ context isAboutBrowserItemType: ClyVariableMethodGroup
]

{ #category : 'execution' }
ClyInvertVariableOrderCommand >> executeRefactoring [

	ClySubclassHierarchy invertVariableHierarchy.
	self context browser switchToVariables
]

{ #category : 'accessing' }
ClyInvertVariableOrderCommand >> name [

	^ 'Invert hierarchy'
]
