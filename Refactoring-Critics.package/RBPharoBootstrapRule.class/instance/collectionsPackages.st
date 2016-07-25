accessing
collectionsPackages
	^ (CollectionsAssembly parts collect: #key)
		reject: [ :each | each = #'Collections-Atomic' ]