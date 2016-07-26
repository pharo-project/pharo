building
compose
	self title: self defaultPlaygroundTitle.
	self act: [:b | b update] icon: GLMUIThemeExtraIcons glamorousRefresh entitled: 'Update'.
	self act: [:b | 
		| tree item |
		tree := (HelpBrowser open instVarNamed: #treeMorph).
		item := tree allItems detect: [:each | 
			each complexContents item key = GTPlaygroundHelp key].
		tree selection: item ] 
		icon: GLMUIThemeExtraIcons glamorousHelp 
		entitled: 'Help'.
	self custom:
			(GTInspector new
				noTitle; 
				noActions;
				showFirst: [ :composite |
					self codeIn: composite ];
				yourself);
		when: GLMBrowserClosing do: [ :ann |
			self actOnBrowserClosing: ann ]