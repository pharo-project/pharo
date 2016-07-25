accessing - tree
labelFor: aPackageOrClass 

	self hasSearch
		ifTrue: [ ^ aPackageOrClass name ].
	
	^ (aPackageOrClass name copyAfterLast: $-)
		ifEmpty: [ aPackageOrClass name ]