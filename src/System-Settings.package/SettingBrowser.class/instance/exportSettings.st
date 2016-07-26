export
exportSettings
	| title nodes actions   |
	
	nodes := self treeHolder nodeList.
	title := 'Exporting settings'.
	title
		displayProgressFrom: 1 
		to: nodes size 
		during: [:bar | actions := nodes collectWithIndex: [:e :i | 
											bar current: i.
											bar label: (String streamContents: [:s | 
															s << title << ' (' << (e item label) << ')']).
											e item exportSettingAction ]].
		
	actions := actions reject: [:e | e isNil ].
	self exportAllSettings: actions by: 50 withBasename: 'exported_settings'.