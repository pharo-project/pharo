morphs
firstMorph

	^ self item 
		ifNotNil: [:it | it first pluginName asStringMorph ]