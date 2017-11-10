Extension { #name : #DoesNotUnderstandDebugAction }

{ #category : #'*GT-Debugger' }
DoesNotUnderstandDebugAction class >> gtStackDebuggingActionFor: aDebugger [
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousAdd;
			order: 4
]
