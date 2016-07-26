private
isAliasSelector: aSymbol
	"Return true if the selector aSymbol is an alias defined
	in this or in another composition somewhere deeper in 
	the tree of traits compositions."

	| methodDescription |
	methodDescription := (self methodDescriptionsForSelector: aSymbol)
		detect: [:each | each selector = aSymbol].
	^methodDescription isAliasSelector