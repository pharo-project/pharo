Extension { #name : #StepThroughDebugAction }

{ #category : #'*GT-Debugger' }
StepThroughDebugAction class >> gtGlobalDebuggingActionFor: aDebugger [
	<gtDebuggingAction>
	
	^ (self forDebugger: aDebugger)
		label: nil
]

{ #category : #'*GT-Debugger' }
StepThroughDebugAction class >> gtStackDebuggingActionFor: aDebugger [
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousThrough
]
