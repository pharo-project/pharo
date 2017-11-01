Extension { #name : #RunToSelectionDebugAction }

{ #category : #'*GT-Debugger' }
RunToSelectionDebugAction class >> gtActionFor: aDebugger [
	"<gtCodeDebuggingAction>"
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousPlay
]
