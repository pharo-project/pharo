registration
gtBasicInspectActionFor: aDebugger
	"<gtContextCodeDebuggingAction>"
	
	^ (self forDebugger: aDebugger)
			makeBasicInspect;
			label: 'Basic inspect it';
			keyText: 'I'; 
			order: 16;
			icon: GLMUIThemeExtraIcons glamorousInspect