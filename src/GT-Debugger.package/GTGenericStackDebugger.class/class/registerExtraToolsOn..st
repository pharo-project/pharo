tools registry
registerExtraToolsOn: registry
	"This adds self in the list of tools that appears in the small debugger"
	registry registerDebugger: self withRank: 3.