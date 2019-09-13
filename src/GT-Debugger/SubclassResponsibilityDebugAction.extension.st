Extension { #name : #SubclassResponsibilityDebugAction }

{ #category : #'*GT-Debugger' }
SubclassResponsibilityDebugAction class >> gtStackDebuggingActionFor: aDebugger [
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousAdd
]
