"
I store values that are settings for extending the debugger with a custom debugging tool.
I only provide accessors to settings for the settings framework to work properly in the SpecDebugger class.
"
Class {
	#name : #StDebuggerExtensionSetting,
	#superclass : #Object,
	#instVars : [
		'showInDebugger',
		'displayOrder',
		'extensionName'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #'instance creation' }
StDebuggerExtensionSetting class >> fromDebuggerExtensionClass: aClass [
	| pragmas |
	pragmas := Pragma
		allNamed: StDebugger extendedToolsPragma
		in: aClass class.
	^ pragmas
		collect: [ :pragma | 
			| setting |
			setting := self new.
			setting extensionName: aClass debuggingExtensionToolName.
			setting displayOrder: (pragma argumentAt: 1).
			setting showInDebugger: (pragma argumentAt: 2).
			setting ]
]

{ #category : #accessing }
StDebuggerExtensionSetting >> displayOrder [
	^ displayOrder
]

{ #category : #accessing }
StDebuggerExtensionSetting >> displayOrder: anObject [
	displayOrder := anObject
]

{ #category : #accessing }
StDebuggerExtensionSetting >> extensionName [
	^ extensionName
]

{ #category : #accessing }
StDebuggerExtensionSetting >> extensionName: anObject [
	extensionName := anObject
]

{ #category : #accessing }
StDebuggerExtensionSetting >> showInDebugger [
	^ showInDebugger
]

{ #category : #accessing }
StDebuggerExtensionSetting >> showInDebugger: anObject [
	showInDebugger := anObject
]
