accessing
settingsKeywords
	^ (PragmaCollector new
		filter: [ :prg | prg selector = #settingPragmaProcessor ]) reset
		collect: [ :p | p method selector ]