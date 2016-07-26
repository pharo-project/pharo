rewrite code
typeClassTextFor: aCollectionOfClass
	^ String streamContents: [:s |
		aCollectionOfClass do: [:class |
			s << (RBRefactoryTyper new runOn: class) printString.
			s cr; cr ]]