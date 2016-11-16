accessing
settingsKeywords 
	^ ((PragmaCollector new filter: [:prg | prg keyword = #settingPragmaProcessor]) reset) collect: [:p | p method selector].
