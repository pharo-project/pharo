generate scripts/methods
currentUpdateVersionNumber
	^ CurrentUpdateVersionNumber ifNil: [SystemVersion current highestUpdate]