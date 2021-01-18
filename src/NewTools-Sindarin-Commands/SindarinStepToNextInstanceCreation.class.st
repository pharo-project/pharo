"
I will step the execution until a class is instantiated
"
Class {
	#name : #SindarinStepToNextInstanceCreation,
	#superclass : #SindarinCommand,
	#instVars : [
		'errorString',
		'maxDepth'
	],
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #initialization }
SindarinStepToNextInstanceCreation class >> defaultIconName [
	^ #smallNew
]

{ #category : #accessing }
SindarinStepToNextInstanceCreation class >> defaultMaxDepth [
	^1000
]

{ #category : #default }
SindarinStepToNextInstanceCreation class >> defaultName [
	<toolbarExtensionDebugCommand: 50>
	<codeExtensionDebugCommand: 50>
	^ 'Next instance creation'
]

{ #category : #accessing }
SindarinStepToNextInstanceCreation >> errorDuringSearchString [

	^ 'Search interrupted due to exception.'
]

{ #category : #accessing }
SindarinStepToNextInstanceCreation >> errorString [
	^errorString
]

{ #category : #executing }
SindarinStepToNextInstanceCreation >> execute [

	| debugger sender depth |
	debugger := self debuggerPresenter sindarinDebugger.
	self debuggerPresenter removeSessionHolderSubscriptions.
	sender := debugger context sender.

	depth := 0.
	errorString := nil.
	debugger step.
	[ debugger isAboutToInstantiateClass or: [ errorString notNil ] ] 
		whileFalse: [ 
			debugger step.
			depth := depth + 1.
			depth = self maxDepth ifTrue: [ errorString := self notFoundErrorString ].
			debugger context == sender ifTrue: [ 
				errorString := self notFoundInCurrentContextString ].
			debugger hasSignalledUnhandledException ifTrue: [ 
				errorString := self errorDuringSearchString ] ].

	self debuggerPresenter
		setSessionHolderSubscriptions;
		forceSessionUpdate.

	errorString ifNotNil: [ 
		self debuggerPresenter
			warning: errorString
			for: self debuggerPresenter toolbar ]
]

{ #category : #accessing }
SindarinStepToNextInstanceCreation >> maxDepth [

	^ maxDepth ifNil: [ maxDepth := self class defaultMaxDepth ]
]

{ #category : #accessing }
SindarinStepToNextInstanceCreation >> maxDepth: anObject [

	maxDepth := anObject
]

{ #category : #accessing }
SindarinStepToNextInstanceCreation >> notFoundErrorString [

	^ 'No instance creation found in 1000 debugger steps: try again to continue your search.'
]

{ #category : #accessing }
SindarinStepToNextInstanceCreation >> notFoundInCurrentContextString [

	^ 'No instance creation found in this context.'
]
