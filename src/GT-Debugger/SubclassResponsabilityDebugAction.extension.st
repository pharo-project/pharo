Extension { #name : #SubclassResponsabilityDebugAction }

{ #category : #'*GT-Debugger' }
SubclassResponsabilityDebugAction class >> gtStackDebuggingActionFor: aDebugger [
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousAdd
]
