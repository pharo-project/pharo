private
packages: aCollection
	| packages |
	packages := OrderedCollection new.
	RPackageOrganizer default packages do: [ :package |
		aCollection do: [ :packageName |
			(package matchesPackagePattern: packageName) ifTrue: [ 
				packages add: package ] ] ].
	^ packages