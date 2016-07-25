printing
printOn: aStream
	| loadedPackageNames |
	super printOn: aStream.
	(loadedPackageNames := self groupContentsIfLoaded) isEmpty
		ifTrue: [ aStream nextPutAll: ' ---' ]
		ifFalse: [ 
			aStream nextPutAll: ' [ '.
			loadedPackageNames do: [:pkgName | aStream nextPutAll: pkgName, ' '  ].
			aStream nextPut: $] ].