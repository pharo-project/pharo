tools registry
registerToolsOn: registry
	"self registerToolsOn: Smalltalk tools" 
	registry register: self as: #codeCompletion
