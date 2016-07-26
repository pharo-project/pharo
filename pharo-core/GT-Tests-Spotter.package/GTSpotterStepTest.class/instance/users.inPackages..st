private-navigation
users: aSelector inPackages: anArray
	| users |
	users := OrderedCollection new.
	RPackageOrganizer default packages do: [ :package |
		anArray do: [ :packageName |
			package packageName = packageName ifTrue: [ 
				package methods do: [ :cm | 
					((cm refersToLiteral: aSelector) 
						and: [ (cm hasPragmaNamed: aSelector) not ]) ifTrue: [ 
							users add: cm ] ] ] ] ].
	^ users