user interface
inputWidgetForPassword
	| widget |
	widget :=  self theme  
				newTextEntryIn: World
				for: self
				get: #realValue
				set: #realValue:
				class: String
				getEnabled: #enabled
				help: nil.
	widget
		wantsFrameAdornments: true;
		hResizing: #rigid;
		width: 450; 
		font: (StrikeFont passwordFontSize:  self theme  textFont pointSize).
	^ widget