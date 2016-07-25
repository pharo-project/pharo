*Tool-Base
browseUndeclaredReferences
	"
	SystemNavigation new browseUndeclaredReferences
	"
	
	Undeclared removeUnreferencedKeys.
	Undeclared associations do: [:binding |
		self
			browseMessageList: (self allReferencesTo: binding )
			name: 'References to Undeclared: ', binding key printString ]