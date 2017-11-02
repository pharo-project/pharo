Extension { #name : #ResumeDebugAction }

{ #category : #'*GT-Debugger' }
ResumeDebugAction class >> gtGlobalDebuggingActionFor: aDebugger [
	<gtDebuggingAction>
	
	^ (self forDebugger: aDebugger)
		label: nil
]

{ #category : #'*GT-Debugger' }
ResumeDebugAction class >> gtStackDebuggingActionFor: aDebugger [
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousPlay
]
