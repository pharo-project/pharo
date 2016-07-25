tools registry
registerToolsOn: registry
	"Add ourselves to registry. See [Smalltalk tools]"
	"self registerToolsOn: Smalltalk tools" 
	registry register: self as: #workspace
