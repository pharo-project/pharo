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
SycBrowseClassCommand >> executeRefactoring [
	targetClass browse
]

{ #category : 'accessing' }
SycBrowseClassCommand >> iconName [
	^#browse
]

{ #category : 'testing' }
SycBrowseClassCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isSourceCodeContext
]

{ #category : 'accessing' }
SycBrowseClassCommand >> menuGroup [

	^ ClyQueryMenuGroup
]

{ #category : 'accessing' }
SycBrowseClassCommand >> name [
	^'Browse'
]

{ #category : 'accessing' }
SycBrowseClassCommand >> order [

	^ 1000
]

{ #category : 'preparation' }
SycBrowseClassCommand >> prepareFullExecution [

	| scope |
	scope := RBBrowserEnvironment new.
	targetClass := (scope at: self context selectedSourceNode name)
		ifNil: [ self context lastSelectedClass ]
]
