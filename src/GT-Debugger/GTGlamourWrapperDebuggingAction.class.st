"
I simply wrap an existing glamour action. Currently I just return the action when one send #asGlamourAction.
"
Class {
	#name : #GTGlamourWrapperDebuggingAction,
	#superclass : #DebugAction,
	#instVars : [
		'glamourAction'
	],
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTGlamourWrapperDebuggingAction class >> gtActionFor: aDebugger [
	<gtContextCodeDebuggingAction>
	|index|
	
	index := 0.
	^ aDebugger codePresentation defaultSelectionActions collect: [ :each | 
			self new 
				glamourAction: each;
				order: (index := index + 10) ]
]

{ #category : #transformation }
GTGlamourWrapperDebuggingAction >> asGlamourAction [
	|actionBlock|
	actionBlock := glamourAction action.
	
	^ glamourAction
		action: [ :presentation :entity |
			self precondition ifTrue: [ 
				self preAction.
				actionBlock cull: presentation cull: entity.
				self postAction ] ]
]

{ #category : #accessing }
GTGlamourWrapperDebuggingAction >> glamourAction: anObject [
	glamourAction := anObject.
]

{ #category : #initialization }
GTGlamourWrapperDebuggingAction >> initialize [
	super initialize.
	self needsUpdate: false.
	self needsValidation: false.
]
