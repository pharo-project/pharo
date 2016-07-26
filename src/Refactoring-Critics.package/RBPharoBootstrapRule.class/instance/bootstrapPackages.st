accessing
bootstrapPackages
	^ ((PharoKernelAssembly parts collect: #key)
		reject: [ :each | each = #Collections ]) asOrderedCollection 
			addAll: self collectionsPackages;
			yourself