running
installMethods
	
	| definitions total |
	definitions := systemDefinition allClassesAndTraits sorted: [ :a :b | a name > b name ].
	total := definitions size.
	definitions withIndexDo: [ :aClassDefinition :index |
		self installMethodsInBehaviorDefinition: aClassDefinition prefixed: (index printPaddedWith: $0 to: 3), '/' , total asString.
	].
	self checkpoint: 'installed_methods'.