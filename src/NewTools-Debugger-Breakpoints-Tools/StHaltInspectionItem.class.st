"
I am a data holder for halts and breakpoints to be displayed in StObjectBreakpointInspection tables.
"
Class {
	#name : #StHaltInspectionItem,
	#superclass : #Object,
	#instVars : [
		'node',
		'type',
		'interval',
		'displayEntity',
		'scope'
	],
	#category : #'NewTools-Debugger-Breakpoints-Tools-Presenters'
}

{ #category : #'instance creation' }
StHaltInspectionItem class >> forBreakpoint: aBreakpoint [ 
	^StBreakpointInspectionItem new forBreakpoint: aBreakpoint 
]

{ #category : #'instance creation' }
StHaltInspectionItem class >> forHalt: aRBMessageNode [ 
	^self new forHalt: aRBMessageNode
]

{ #category : #control }
StHaltInspectionItem >> disableInContext: aStHaltAndBreakpointController [ 
	aStHaltAndBreakpointController disableHaltNode: node
]

{ #category : #accessing }
StHaltInspectionItem >> displayEntity [
	^ displayEntity
]

{ #category : #control }
StHaltInspectionItem >> enableInContext: aStHaltAndBreakpointController [ 
	aStHaltAndBreakpointController enableHaltNode: node
]

{ #category : #accessing }
StHaltInspectionItem >> entityToBrowse [

	^ node methodNode method
]

{ #category : #initialization }
StHaltInspectionItem >> forHalt: aRBMessageNode [ 
	node := aRBMessageNode.
	type := Halt name asString.
	interval := node start to: node stop.
	scope := aRBMessageNode methodNode methodClass.
	displayEntity := aRBMessageNode methodNode
]

{ #category : #accessing }
StHaltInspectionItem >> interval [
	^ interval
]

{ #category : #accessing }
StHaltInspectionItem >> interval: anObject [
	interval := anObject
]

{ #category : #control }
StHaltInspectionItem >> isEnabledInContext: aStHaltAndBreakpointController [ 
	^aStHaltAndBreakpointController isHaltNodeEnabled: node
]

{ #category : #accessing }
StHaltInspectionItem >> node [
	^ node
]

{ #category : #accessing }
StHaltInspectionItem >> node: anObject [
	node := anObject
]

{ #category : #printing }
StHaltInspectionItem >> printContents [
	^displayEntity sourceCode
]

{ #category : #printing }
StHaltInspectionItem >> printEntity [
	^displayEntity method printString
]

{ #category : #printing }
StHaltInspectionItem >> printScope [

	^ scope printString
]

{ #category : #updating }
StHaltInspectionItem >> removeInPresenterContext: aStObjectBreakpointInspection [ 
	self inform: 'Edit your code to remove halts'
]

{ #category : #accessing }
StHaltInspectionItem >> scope [
	^ scope
]

{ #category : #accessing }
StHaltInspectionItem >> scope: anObject [
	scope := anObject
]

{ #category : #accessing }
StHaltInspectionItem >> type [
	^ type
]

{ #category : #accessing }
StHaltInspectionItem >> type: anObject [
	type := anObject
]

{ #category : #updating }
StHaltInspectionItem >> updateSourceCodePaneFor: aStObjectBreakpointInspection [ 
	aStObjectBreakpointInspection updateSourceCodeForHaltItem: self
]
