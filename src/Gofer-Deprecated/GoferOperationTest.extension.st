Extension { #name : 'GoferOperationTest' }

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testCleanup [

	| class |
	gofer
		package: 'GoferFoo';
		load.
	class := environment classNamed: #GoferFoo.
	environment organization ensureTag: #Empty inPackage: #GoferFoo.
	class addProtocol: #empty.
	class class addProtocol: #empty.
	gofer cleanup.
	self deny: (self packageOrganizer hasTag: #Empty inPackage: #GoferFoo).
	self deny: (class hasProtocol: #'GoferFoo-Empty').
	self deny: (class class hasProtocol: #'GoferFoo-Empty')
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testCommit [

	| repository |
	repository := MCDictionaryRepository new.
	gofer
		package: 'GoferFoo';
		load.
	gofer := Gofer new.
	gofer
		disablePackageCache;
		repository: repository;
		package: 'GoferFoo';
		commit: 'A test commit'.
	self assert: repository allVersionInfos size equals: 1
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testFetch [

	gofer
		package: 'GoferFoo';
		fetch
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testLocalChanges [

	| changes |
	gofer
		package: 'GoferBar';
		load.
	(environment classNamed: #GoferBar) compile: 'foo'.
	changes := gofer localChanges.
	self assert: changes operations size equals: 1
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testMerge [

	| initial |
	initial := gofer copy.
	initial
		version: 'GoferBar-jf.1';
		load.
	gofer
		package: 'GoferBar';
		load.
	(environment classNamed: #GoferBar) compile: 'foo'.
	[ gofer merge ]
		on: ProvideAnswerNotification
		do: [ :e | e resume: true ].
	self assert: (self hasClass: #GoferBar selector: #foo)
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testPush [

	| repository |
	gofer := Gofer new.
	gofer disablePackageCache.
	gofer repository: (repository := MCDictionaryRepository new).
	gofer package: 'GoferFoo'.
	gofer push
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testRecompile [

	gofer package: 'Gofer-Core'.
	gofer recompile
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testReinitialize [

	| class |
	gofer
		package: 'GoferFoo';
		load.
	class := Smalltalk classNamed: #GoferFoo.
	class
		addClassVarNamed: #InstanceSide;
		addClassVarNamed: #ClassSide.
	class compile: 'initialize InstanceSide := true'.
	class class compile: 'initialize ClassSide := true'.
	self assert: (class classVariableNamed: #InstanceSide) read isNil.
	self assert: (class classVariableNamed: #ClassSide) read isNil.
	gofer reinitialize.
	self assert: (class classVariableNamed: #InstanceSide) read isNil.
	self assert: (class classVariableNamed: #ClassSide) read
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testRemoteChanges [

	| changes |
	gofer
		package: 'GoferBar';
		load.
	(environment classNamed: #GoferBar) compile: 'foo'.
	changes := gofer remoteChanges.
	self assert: changes operations size equals: 1
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testRevert [

	gofer
		package: 'GoferFoo';
		package: 'GoferBar';
		load.
	(environment classNamed: #GoferBar) package: 'GoferFoo'.
	gofer revert.
	self assert: (environment classNamed: #GoferFoo) package name equals: #GoferFoo.
	self assert: (environment classNamed: #GoferBar) package name equals: #GoferBar
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testUnload [

	gofer
		package: 'GoferFoo';
		load.
	gofer unload.
	self deny: (self hasPackage: 'GoferFoo').
	self deny: (self hasClass: #GoferFoo)
]

{ #category : '*Gofer-Deprecated' }
GoferOperationTest >> testUpdate [

	| initial |
	initial := gofer copy.
	initial
		version: 'GoferFoo-lr.1';
		load.
	gofer package: 'GoferFoo'.
	gofer update.
	self assert: (self hasVersion: 'GoferFoo-lr.4')
]
