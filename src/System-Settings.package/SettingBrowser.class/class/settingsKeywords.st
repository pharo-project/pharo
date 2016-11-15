accessing
settingsKeywords
	^ (PragmaCollector new
		filter: [ :prg | prg methodSelector = #settingPragmaProcessor ])
		reset collect: [ :p | p method selector ]