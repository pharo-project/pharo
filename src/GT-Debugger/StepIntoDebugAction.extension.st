Extension { #name : #StepIntoDebugAction }

{ #category : #'*GT-Debugger' }
StepIntoDebugAction class >> gtGlobalDebuggingActionFor: aDebugger [
	<gtDebuggingAction>
	
	^ (self forDebugger: aDebugger)
		label: nil
]

{ #category : #'*GT-Debugger' }
StepIntoDebugAction class >> gtStackDebuggingActionFor: aDebugger [
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousInto
]
