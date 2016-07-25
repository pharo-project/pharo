private-navigation
implementors: aSelector inPackages: anArray
	| implementors |
	implementors := OrderedCollection new.
	RPackageOrganizer default packages do: [ :package |
		anArray do: [ :packageName |
			package packageName = packageName ifTrue: [ 
				package methods do: [ :cm | 
					cm selector = implementors ifTrue: [ implementors add: cm ] ] ] ] ].
	^ implementors