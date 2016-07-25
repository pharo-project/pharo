accessing
dependenciesFrom: aClass
	| dependenciesFrom |
	dependenciesFrom := OrderedCollection new.
	self dependencies
		do: [ :compositeDependency | 
				dependenciesFrom addAll: (compositeDependency dependenciesFrom: aClass) ].
	^ dependenciesFrom