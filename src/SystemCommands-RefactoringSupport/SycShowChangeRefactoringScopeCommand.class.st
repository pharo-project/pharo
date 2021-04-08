"
I am a command to change the default refactoring scope
"
Class {
	#name : #SycShowChangeRefactoringScopeCommand,
	#superclass : #SycClassCommand,
	#instVars : [
		'browser'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #activation }
SycShowChangeRefactoringScopeCommand class >> fullBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation 
		byItemOf: ClyButtonToolbarGroup 
		order: self toolbarOrder
		for: ClyFullBrowserPackageContext
]

{ #category : #activation }
SycShowChangeRefactoringScopeCommand class >> fullBrowserToolbarActivation2 [
	<classAnnotation>
	
	^ClyToolbarActivation 
		byItemOf: ClyButtonToolbarGroup 
		order: self toolbarOrder
		for: ClyFullBrowserClassGroupContext
]

{ #category : #activation }
SycShowChangeRefactoringScopeCommand class >> fullBrowserToolbarActivation3 [
	<classAnnotation>
	
	^ClyToolbarActivation 
		byItemOf: ClyButtonToolbarGroup 
		order: self toolbarOrder
		for: ClyFullBrowserProjectContext
]

{ #category : #activation }
SycShowChangeRefactoringScopeCommand class >> toolbarOrder [
	^1
]

{ #category : #accessing }
SycShowChangeRefactoringScopeCommand >> defaultMenuItemName [
	^'Ref. Scope'
]

{ #category : #execution }
SycShowChangeRefactoringScopeCommand >> execute [
	| newScope values |
	values := RefactoringSettings domainValues.
	newScope :=  UIManager default
				chooseFrom: ( values collect: #key )
				values: ( values collect: #value )
				title: 'Choose the default scope'.
			newScope ifNil: [ 
				self inform: 'Command aborted, you must select a scope.'.
			CmdCommandAborted signal ].
	RefactoringSettings scopeRefactoring: newScope
]

{ #category : #execution }
SycShowChangeRefactoringScopeCommand >> isAppliedToBrowser [

	^ false
]

{ #category : #execution }
SycShowChangeRefactoringScopeCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	browser := aToolContext browser
]
