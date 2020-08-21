Trait {
	#name : #TStDebuggerExtension,
	#classInstVars : [
		'showDebuggerExtension',
		'debuggerExtensionDisplayOrder'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #'debugger extension' }
TStDebuggerExtension classSide >> defaultDisplayOrder [

	^ 1
]

{ #category : #'debugger extension' }
TStDebuggerExtension classSide >> displayOrder [

	^ debuggerExtensionDisplayOrder ifNil: [ 
		  debuggerExtensionDisplayOrder := self defaultDisplayOrder ]
]

{ #category : #'debugger extension' }
TStDebuggerExtension classSide >> displayOrder: anInteger [
	debuggerExtensionDisplayOrder := anInteger
]

{ #category : #'debugger extension' }
TStDebuggerExtension classSide >> showInDebugger [
	^showDebuggerExtension ifNil: [showDebuggerExtension := false]
]

{ #category : #'debugger extension' }
TStDebuggerExtension classSide >> showInDebugger: aBoolean [
	showDebuggerExtension := aBoolean
]

{ #category : #'debugger extension' }
TStDebuggerExtension >> debuggerExtensionToolName [
	^self explicitRequirement 
]

{ #category : #'debugger extension' }
TStDebuggerExtension >> displayOrder [
	^self class displayOrder
]
