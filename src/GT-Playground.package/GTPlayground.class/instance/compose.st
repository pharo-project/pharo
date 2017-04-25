building
compose
	self title: self defaultPlaygroundTitle.
	self act: [:b | b update] icon: GLMUIThemeExtraIcons glamorousRefresh entitled: 'Update'.
	self act: [:b | 
		HelpBrowser open selectTopicSatisfying: [:each |
			each owner notNil and:  [
			(each owner key = GTPlaygroundHelp key) and: [
				each title = 'Overview' ] ] ] ] 
		icon: GLMUIThemeExtraIcons glamorousHelp 
		entitled: 'Help'.
	self custom:
			(GTInspector new
				noTitle; 
				noActions;
				wantsAutomaticRefresh: true;
				showFirst: [ :composite |
					self codeIn: composite ];
				yourself);
		when: GLMBrowserClosing do: [ :ann |
			self actOnBrowserClosing: ann ].
	
	GTInspector isStepRefreshEnabled ifTrue: [
		self wantsAutomaticRefresh: true.
		self wantsSteps: true.
		self stepTime: GTInspector stepRefreshRate ]
	