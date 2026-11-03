"
I am a command to open browser on given class
"
Class {
	#name : 'SycBrowseClassCommand',
	#superclass : 'SycSingleClassCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycBrowseClassCommand >> execute [
	targetClass browse
]

{ #category : 'accessing' }
SycBrowseClassCommand >> iconName [
	^#browse
]

{ #category : 'accessing' }
SycBrowseClassCommand >> name [
	^'Browse'
]

{ #category : 'execution' }
SycBrowseClassCommand >> prepareFullExecutionInContext: aToolContext [

	| scope |
	scope := RBBrowserEnvironment new.
	super prepareFullExecutionInContext: aToolContext.
	targetClass := (scope at: aToolContext selectedSourceNode name) 
		ifNil: [ aToolContext lastSelectedClass ]
]
