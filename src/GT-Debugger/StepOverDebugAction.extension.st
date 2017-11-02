Extension { #name : #StepOverDebugAction }

{ #category : #'*GT-Debugger' }
StepOverDebugAction class >> gtGlobalDebuggingActionFor: aDebugger [
	<gtDebuggingAction>
	
	^ (self forDebugger: aDebugger)
		label: nil
]

{ #category : #'*GT-Debugger' }
StepOverDebugAction class >> gtStackDebuggingActionFor: aDebugger [
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousOver
]
