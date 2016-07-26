generate scripts/methods
setUpdateAndScriptVersionNumbers
	CurrentUpdateVersionNumber := self getLatestUpdateNumber + 1.
	CurrentScriptVersionNumber := self getLatestScriptNumber + 1.