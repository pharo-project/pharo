registration
gtBasicInspectActionFor: aDebugger
	"<gtContextCodeDebuggingAction>"
	
	^ (self forDebugger: aDebugger)
			makeBasicInspect;
			label: 'Basic inspect it';
			keymap: $i meta shift; 
			order: 16;
			icon: GLMUIThemeExtraIcons glamorousInspect