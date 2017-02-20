composition
childrenNodesFor: aVariablesBrowser
	"Return a collection of subvariable nodes to be displayed in a given raw variables browser.
	Note: by default we dispatch to browser and let it perform collection"
	<return: #Collection of: #GTInspectorVariableNode>

	^ aVariablesBrowser variableNodesFor: self value