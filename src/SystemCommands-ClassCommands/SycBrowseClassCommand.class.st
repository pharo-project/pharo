"
I am a command to open browser on given class
"
Class {
	#name : 'SycBrowseClassCommand',
	#superclass : 'SycSingleClassCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycBrowseClassCommand >> defaultMenuIconName [
	^#browse
]

{ #category : 'accessing' }
SycBrowseClassCommand >> defaultMenuItemName [
	^'Browse'
]

{ #category : 'execution' }
SycBrowseClassCommand >> execute [
	targetClass browse
]

{ #category : 'execution' }
SycBrowseClassCommand >> prepareFullExecutionInContext: aToolContext [

	| scope |
	scope := RBBrowserEnvironment new.
	super prepareFullExecutionInContext: aToolContext.
	targetClass := (scope at: aToolContext selectedSourceNode name) 
		ifNil: [ aToolContext lastSelectedClass ]
]
