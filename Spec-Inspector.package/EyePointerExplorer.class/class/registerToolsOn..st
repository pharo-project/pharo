as yet unclassified
registerToolsOn: registry
	"Add ourselves to registry. See [Smalltalk tools class]
	self registerToolsOn: Smalltalk tools
	" 
	registry register: self as: #pointerExplorer
