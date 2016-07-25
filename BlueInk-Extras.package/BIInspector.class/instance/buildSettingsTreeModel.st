item creation
buildSettingsTreeModel
	| settingsTreeModel |
	settingsTreeModel := self newTree.
	settingsTreeModel
		roots:
			((SettingTree acceptableKeywords: #(#systemsettings)) nodeList
				select: [ :node | node parentName = #blueInkFormatter ]).
	settingsTreeModel displayBlock: [ :node | self displayNodeFor: node ].
	^ settingsTreeModel