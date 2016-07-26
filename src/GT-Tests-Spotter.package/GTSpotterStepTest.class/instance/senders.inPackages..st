private-navigation
senders: aSelector inPackages: anArray
	| senders |
	senders := OrderedCollection new.
	RPackageOrganizer default packages do: [ :package |
		anArray do: [ :packageName |
			package packageName = packageName ifTrue: [ 
				package methods do: [ :cm | 
					(cm refersToLiteral: aSelector) ifTrue: [ 
						senders add: cm ] ] ] ] ].
	^ senders