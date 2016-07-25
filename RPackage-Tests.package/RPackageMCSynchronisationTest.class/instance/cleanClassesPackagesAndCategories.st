setup
cleanClassesPackagesAndCategories

		|mCPackage|
		Smalltalk removeClassNamed: 'NewClass'.
		Smalltalk removeClassNamed: 'RPackageNewStubClass'.
		Smalltalk removeClassNamed: 'RPackageOldStubClass'.
		Smalltalk removeClassNamed: 'Foo'.
		Smalltalk removeClassNamed: 'FooOther'.
		Smalltalk removeClassNamed: 'NewTrait'.
		Smalltalk removeClassNamed: 'ClassInYPackage'.
		Smalltalk removeClassNamed: 'ClassInZPackage'.
		Smalltalk organization removeCategory: 'Zork'.
		Smalltalk organization removeCategory: 'XXXXX'.
		Smalltalk organization removeCategory: 'XXXXX-YYYY'.
		Smalltalk organization removeCategory: 'XXXX'.
		Smalltalk organization removeCategory: 'YYYYY'.
		Smalltalk organization removeCategory: 'ZZZZZ'.
		Smalltalk organization removeCategory: 'FooPackage-Core'.
		Smalltalk organization removeCategory: 'FooPackage-Other'.
		Smalltalk organization removeCategory: 'FooPackage'.
		Smalltalk organization removeCategory: 'OriginalCategory'.
		Smalltalk organization removeCategory: 'NewCategoryName'.
		Smalltalk organization removeCategory: 'Y'.
		mCPackage := (self allManagers detect: [:each | each packageName = 'OriginalCategory'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'XXXXX'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'XXXX'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'YYYYY'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'YYYY'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'Yyyyy'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].	
		mCPackage := (self allManagers detect: [:each | each packageName = 'YyYyY'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'Y'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'ZZZZZ'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].	
		mCPackage := (self allManagers detect: [:each | each packageName = 'Zzzzz'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'FooPackage-Core'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'FooPackage-Other'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'FooPackage'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
		mCPackage := (self allManagers detect: [:each | each packageName = 'Zork'] ifNone: [nil]) .
		mCPackage ifNotNil: [mCPackage unregister].
			