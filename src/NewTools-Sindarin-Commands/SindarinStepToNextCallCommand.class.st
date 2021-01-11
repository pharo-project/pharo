Class {
	#name : #SindarinStepToNextCallCommand,
	#superclass : #SindarinCommand,
	#instVars : [
		'debugger',
		'conditionValue'
	],
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #initialization }
SindarinStepToNextCallCommand class >> defaultIconName [
	^ #glamorousInto
]

{ #category : #hooks }
SindarinStepToNextCallCommand >> execute [

	| callingMethod maxDepth depth |
	self context removeSessionHolderSubscriptions.
	debugger := self context sindarinDebugger.
	self setConditionValue.
	callingMethod := debugger method.
	debugger step.

	maxDepth := 1000.
	depth := 0.
	[ 
	debugger method ~= callingMethod and: [ 
		depth < maxDepth and: [ self targetCondition ] ] ] 
		whileTrue: [ 
			debugger step.
			depth := depth + 1.
			debugger hasSignalledUnhandledException ifTrue: [ depth := 1000 ] ].

	self context
		setSessionHolderSubscriptions;
		forceSessionUpdate
]

{ #category : #hooks }
SindarinStepToNextCallCommand >> setConditionValue [

	^ self subclassResponsibility
]

{ #category : #hooks }
SindarinStepToNextCallCommand >> targetCondition [

	^ self subclassResponsibility
]
