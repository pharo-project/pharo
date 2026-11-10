"
I test MetacelloInteractiveResolver, and the engine resolving, recording and loading from a resolution, with Metacello's test baselines already in the image (BaselineOfTestToLock1 requires TestToLock2), so nothing is fetched.
"
Class {
	#name : 'MetacelloInteractiveResolverTest',
	#superclass : 'MetacelloTestCase',
	#category : 'Metacello-Tests-Resolution',
	#package : 'Metacello-Tests',
	#tag : 'Resolution'
}

{ #category : 'private' }
MetacelloInteractiveResolverTest >> lock2Registration [

	^ MetacelloProjectRegistration projectSpec: ((MetacelloProjectSpec project: MetacelloProject new)
			   name: 'TestToLock2';
			   className: 'BaselineOfTestToLock2';
			   yourself)
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> resolveDiamondOnConflict: aBlock [
	"TestDiamond reaches TestToLock2 twice: from http://v1.com (Left, first) and from http://v2.com (Right). aBlock handles the conflict between them"

	^ (Metacello new
		   baseline: 'TestDiamond';
		   repository: 'http://repo.com';
		   onConflict: aBlock;
		   resolve) projects detect: [ :each | each name = 'TestToLock2' ]
]

{ #category : 'private' }
MetacelloInteractiveResolverTest >> resolveLock1Loading: aCollection [

	| spec |
	spec := (MetacelloProjectSpec project: MetacelloProject new)
		        name: 'TestToLock1';
		        className: 'BaselineOfTestToLock1';
		        repository: 'http://repo.com';
		        yourself.
	^ MetacelloInteractiveResolver new resolve: spec loads: aCollection engine: MetacelloScriptEngine new
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testAGroupAskedLaterIsRecordedWhereItIsAsked [
	"TestGroups asks TestGroupsShared for Core, then TestGroupsLate asks it for Tests: its tests come after LatePackage, as Monticello's tests come after SUnit in Pharo's build"

	| names |
	names := OrderedCollection new.
	(Metacello new
		 baseline: 'TestGroups';
		 repository: 'http://repo.com';
		 record) packageDirectivesDo: [ :each | names add: each spec name ].

	self assert: names asArray equals: #( 'SharedCore' 'LatePackage' 'SharedTests' 'LateTests' )
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testALoadInstallsTheBaselinesTheResolverDidNotInstall [

	| loader |
	loader := MetacelloTestLoader new.
	Metacello new
		baseline: 'TestToLock1';
		repository: 'http://repo.com';
		loader: loader;
		resolver: MetacelloBaselineReadingResolver new;
		load.

	self
		assert: (loader loaded collect: [ :each | each key ]) asArray
		equals: #( 'Package1' 'TestToLockPackage1' 'TestToLockPackage2' 'BaselineOfTestToLock2' )
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testALoadRegistersEveryProjectAsLoaded [

	Metacello new
		baseline: 'TestToLock1';
		repository: 'http://repo.com';
		loader: MetacelloTestLoader new;
		load.

	#( 'BaselineOfTestToLock1' 'BaselineOfTestToLock2' ) do: [ :each |
			self assert:
				(MetacelloProjectRegistry
					 registrationNamed: each
					 ifPresent: [ :registration | registration loadedInImage ]
					 ifAbsent: [ false ]) ]
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testALoadRegistersTheGroupsLoaded [

	Metacello new
		baseline: 'TestToLock1';
		repository: 'http://repo.com';
		loader: MetacelloTestLoader new;
		load.

	self
		assertCollection: (MetacelloProjectRegistry
				 registrationNamed: 'BaselineOfTestToLock2'
				 ifPresent: [ :registration | registration projectSpec loads ]
				 ifAbsent: [ nil ])
		hasSameElements: #( 'TestToLockPackage1' 'TestToLockPackage2' )
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testALoadWithResolutionErrorsLoadsNothing [

	| loader error |
	loader := MetacelloTestLoader new.
	error := [
		         Metacello new
			         baseline: 'TestToLock1';
			         repository: 'http://repo.com';
			         loader: loader;
			         resolver: MetacelloFailingResolver new;
			         load.
		         nil ]
		         on: MetacelloResolutionError
		         do: [ :e | e ].

	self assert: error notNil.
	self assert: (error resolution errors collect: [ :each | each text ]) asArray equals: #( 'cannot resolve' ).
	self assert: loader loaded isEmpty.
	self assert: MetacelloProjectRegistry registry isEmpty
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testAProjectLoadedIsInstalled [

	self assert: (self lock2Registration
			 loadedInImage: true;
			 yourself) isInstalled
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testAProjectLoadedIsNotInstalledByTheLegacyRule [

	self deny: (self lock2Registration
			 loadedInImage: true;
			 yourself) isInstalledByLegacyRule
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testAProjectRegisteredWithoutItsPackagesIsNotInstalled [
	"TestToLockPackage1 and TestToLockPackage2 are not in the image"

	self deny: self lock2Registration isInstalled
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testKeepsWhoRequiredAProject [

	| lock2 |
	lock2 := (self resolveLock1Loading: #(  )) projects detect: [ :each | each name = 'TestToLock2' ].

	self assert: (lock2 requests collect: [ :each | each requesterName ]) asArray equals: #( 'TestToLock1' ).
	self assertCollection: lock2 loadList hasSameElements: #( 'TestToLockPackage1' 'TestToLockPackage2' )
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testLeavesLockedProjectsOut [

	Metacello new
		baseline: 'TestToLock2';
		repository: 'http://repo.com';
		lock.

	self assert: ((self resolveLock1Loading: #(  )) projects collect: [ :each | each name ]) asArray equals: #( 'TestToLock1' )
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testLegacyAProjectRegisteredWithItsPackagesInTheImageIsInstalled [
	"Metacello's own packages are in the image"

	| registration |
	registration := MetacelloProjectRegistration projectSpec: ((MetacelloProjectSpec project: MetacelloProject new)
			                 name: 'Metacello';
			                 className: 'BaselineOfMetacello';
			                 yourself).

	self assert: registration isInstalled.
	self assert: registration isInstalledByLegacyRule
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testOnlyTheGroupsAskedReachProjects [
	"Package1 alone requires no project"

	self
		assert: ((self resolveLock1Loading: #( 'Package1' )) projects collect: [ :each | each name ]) asArray
		equals: #( 'TestToLock1' )
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testRecordingFollowsTheResolution [

	| names |
	names := OrderedCollection new.
	(Metacello new
		 baseline: 'TestToLock1';
		 repository: 'http://repo.com';
		 record) packageDirectivesDo: [ :each | names add: each spec name ].

	self assert: names asArray equals: #( 'Package1' 'TestToLockPackage1' 'TestToLockPackage2' )
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testRegistersTheProjectsItChooses [

	self resolveLock1Loading: #(  ).

	self assert:
		(MetacelloProjectRegistry registrationNamed: 'BaselineOfTestToLock2' ifPresent: [ :each | true ] ifAbsent: [ false ])
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testResolveAnswersAResolutionWithErrors [

	| resolution |
	resolution := Metacello new
		              baseline: 'TestToLock1';
		              repository: 'http://repo.com';
		              resolver: MetacelloFailingResolver new;
		              resolve.

	self assert: resolution hasErrors
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testResolveAnswersTheResolution [

	| resolution |
	resolution := Metacello new
		              baseline: 'TestToLock1';
		              repository: 'http://repo.com';
		              resolve.

	self assertCollection: (resolution projects collect: [ :each | each name ]) hasSameElements: #( 'TestToLock1' 'TestToLock2' )
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testResolveRegistersNothing [

	Metacello new
		baseline: 'TestToLock1';
		repository: 'http://repo.com';
		resolve.

	self assert: MetacelloProjectRegistry registry isEmpty
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testResolvesTheRootAndWhatItRequires [

	| resolution |
	resolution := self resolveLock1Loading: #(  ).

	self assert: resolution root name equals: 'TestToLock1'.
	self assertCollection: (resolution projects collect: [ :each | each name ]) hasSameElements: #( 'TestToLock1' 'TestToLock2' ).
	self deny: resolution hasErrors
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testTheHandlersCanKeepTheFirstVersionAsked [

	| asked lock2 |
	asked := false.
	lock2 := self resolveDiamondOnConflict: [ :ex |
			         asked := true.
			         ex useLoaded ].

	self assert: asked.
	self assert: lock2 projectSpec repositoryDescriptions asArray equals: #( 'http://v1.com' )
]

{ #category : 'tests' }
MetacelloInteractiveResolverTest >> testTheHandlersChooseBetweenTwoVersionsAsked [

	| lock2 |
	lock2 := self resolveDiamondOnConflict: [ :ex | ex useIncoming ].

	self assert: lock2 projectSpec repositoryDescriptions asArray equals: #( 'http://v2.com' ).
	self
		assertCollection: (lock2 requests collect: [ :each | each requesterName ])
		hasSameElements: #( 'TestDiamondLeft' 'TestDiamondRight' )
]
