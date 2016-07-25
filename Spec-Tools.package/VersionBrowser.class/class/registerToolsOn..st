tool registry
registerToolsOn: registry
	"Add ourselves to registry. See [Smalltalk tools]" 
	registry register: self as: #versionBrowser
