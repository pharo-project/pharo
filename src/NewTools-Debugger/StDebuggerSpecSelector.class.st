"
I select and returns the spec selector to be used by the StDebugger presenter.
	Two cases:
	- current selector exists or is not valid: returns default spec selector with debugger extensions
	- current selector exists: select a version with or without debugger extensions
	
Three cases:
	- custom selector: returned as is
	- selector is one of the display strategy with or without debugger extensions
		- extensions activated: force return of spec selector with extensions
		- extensions not activated: force return of spec selector without extensions
		
	Checks performed to tell if extensions are activated:
		- activateDebuggerExtensions must be true
		- AND at least one existing extension class is activated as debugger extension
"
Class {
	#name : #StDebuggerSpecSelector,
	#superclass : #Object,
	#instVars : [
		'specSelector'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerSpecSelector class >> debuggerClass [ 	
	^StDebugger
]

{ #category : #'default spec selectors' }
StDebuggerSpecSelector class >> defaultDebuggerWithExtensionSpecSelector [
	^#debuggerWithExtensionsSpec
]

{ #category : #'default spec selectors' }
StDebuggerSpecSelector class >> defaultDebuggerWithoutExtensionSpecSelector [
	^#debuggerWithoutExtensionsSpec
]

{ #category : #'instance creation' }
StDebuggerSpecSelector class >> defaultSelector [
	^self on: self defaultDebuggerWithExtensionSpecSelector
]

{ #category : #'instance creation' }
StDebuggerSpecSelector class >> on: aSelector [
	^self new specSelector: aSelector
]

{ #category : #accessing }
StDebuggerSpecSelector >> debuggerClass [ 
	^self class debuggerClass
]

{ #category : #'specs - selectors' }
StDebuggerSpecSelector >> isDefaultSelector: aSelector [
	^ {self class defaultDebuggerWithExtensionSpecSelector.
	self class defaultDebuggerWithoutExtensionSpecSelector}
		includes: aSelector
]

{ #category : #'specs - selectors' }
StDebuggerSpecSelector >> selectExtensionActivationSpecFrom: aDefaultSpecSelector [
	"Altered depending on the current configuration of the debugger extensions.
	The selection just adds or remove the extensions from the spec selector"

	self class defaultDebuggerWithoutExtensionSpecSelector
		= aDefaultSpecSelector
		ifTrue: [ ^ aDefaultSpecSelector ].
	^ self
		selectorForDebuggerStatus: self debuggerClass activateDebuggerExtensions
		extensionsStatus: self debuggerClass hasAnyActivatedExtension
]

{ #category : #'specs - selectors' }
StDebuggerSpecSelector >> selectSpecSelector [
	"Sets and returns the spec selector to be use by the presenter.
	Two cases:
	- current selector exists or is not valid: returns default spec selector with extensions
	- current selector exists: select a version with or without debugger extensions"

	| selector |
	selector := (self debuggerClass availableSpecSelectors
		includes: specSelector)
		ifTrue: [ specSelector ]
		ifFalse: [ self class defaultDebuggerWithExtensionSpecSelector ].
	(self isDefaultSelector: selector) ifFalse:[^selector].
	^ self selectExtensionActivationSpecFrom: selector
]

{ #category : #'specs - selectors' }
StDebuggerSpecSelector >> selectorForDebuggerStatus: debuggerExtensionsActivated extensionsStatus: debuggerHasExtensions [
	"This is only relevant in the case we use default StDebugger specs for which the extension column can be configured in settings"

	^ (debuggerExtensionsActivated and: [ debuggerHasExtensions ])
		ifTrue: [ self class defaultDebuggerWithExtensionSpecSelector ]
		ifFalse: [ self class defaultDebuggerWithoutExtensionSpecSelector ]
]

{ #category : #'default spec selectors' }
StDebuggerSpecSelector >> specSelector [
	^ specSelector
]

{ #category : #accessing }
StDebuggerSpecSelector >> specSelector: anObject [
	specSelector := anObject
]
