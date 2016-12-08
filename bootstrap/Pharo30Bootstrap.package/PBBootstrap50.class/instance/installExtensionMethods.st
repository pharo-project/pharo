running
installExtensionMethods
	
	| definitions total |
	definitions := self systemDefinition packages sorted: [ :a :b | a name < b name ].
	total := definitions size.
	definitions withIndexDo: [ :aPackageDefinition :index |
		self installExtensionMethodsOf: aPackageDefinition prefixed: (index printPaddedWith: $0 to: 3), '/' , total asString.
	].
	self checkpoint: 'installed_extension_methods'.