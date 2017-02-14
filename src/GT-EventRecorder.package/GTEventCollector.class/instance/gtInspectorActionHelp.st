gt-inspector-extension
gtInspectorActionHelp
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [:b | 
		HelpBrowser open selectTopicSatisfying: [:each |
			each owner notNil and:  [
			(each owner key = GTEventRecorderHelp key) and: [
				each title = 'Client' ] ] ] ];
		icon: GLMUIThemeExtraIcons glamorousHelp;
		title: 'Help'