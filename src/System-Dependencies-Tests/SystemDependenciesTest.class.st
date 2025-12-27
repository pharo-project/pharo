"
This test checks dependencies in the image and proves that there are no new unknown dependencies between packages.

It is not a standard unit test because to compute dependencies takes a lot of time. It uses cached version of the dependenices report.

"
Class {
	#name : 'SystemDependenciesTest',
	#superclass : 'TestCase',
	#category : 'System-Dependencies-Tests',
	#package : 'System-Dependencies-Tests'
}

{ #category : 'accessing' }
SystemDependenciesTest class >> defaultTimeLimit [

	^ 30 seconds
]

{ #category : 'accessing' }
SystemDependenciesTest class >> resources [

	^ super resources copyWith: SystemDependenciesReportResource
]

{ #category : 'packages' }
SystemDependenciesTest >> _01kernelLayerPackages [

	^ BaselineOfKernel corePackageNames asSet
]

{ #category : 'packages' }
SystemDependenciesTest >> _02kernelAdditionalPackagesLayerPackages [

	^ self _01kernelLayerPackages , BaselineOfKernel additionalPackagesPackageNames
]

{ #category : 'packages' }
SystemDependenciesTest >> _03compilerLayerPackages [

	^ self _02kernelAdditionalPackagesLayerPackages , BaselineOfCompiler corePackageNames
]

{ #category : 'packages' }
SystemDependenciesTest >> _04fileSystemLayerPackages [

	^ self _03compilerLayerPackages , BaselineOfFileSystem corePackageNames
]

{ #category : 'packages' }
SystemDependenciesTest >> _05traitsLayerPackages [

	^ self _04fileSystemLayerPackages , BaselineOfTraits corePackageNames
]

{ #category : 'packages' }
SystemDependenciesTest >> _06monticelloCoreLayerPackages [

	^ self _05traitsLayerPackages , BaselineOfMonticello corePackageNames
]

{ #category : 'packages' }
SystemDependenciesTest >> _07metacelloCoreLayerPackages [

	^ self _06monticelloCoreLayerPackages , (BaselineOfMetacello packagesOfGroupNamed: 'Core') , (BaselineOfTonel packagesOfGroupNamed: #core)
]

{ #category : 'packages' }
SystemDependenciesTest >> _08monticelloLayerPackages [

	^ self _07metacelloCoreLayerPackages , BaselineOfMonticello remoteRepositoriesPackageNames
]

{ #category : 'packages' }
SystemDependenciesTest >> _09sunitCoreLayerPackages [

	^ self _08monticelloLayerPackages , BaselineOfSUnit corePackageNames
]

{ #category : 'packages' }
SystemDependenciesTest >> _10debuggingLayerPackages [

	^ self _09sunitCoreLayerPackages , BaselineOfDebugging corePackageNames
]

{ #category : 'accessing' }
SystemDependenciesTest >> externalDependendiesOf: packagesCollection [

	| dependencies |

	dependencies := packagesCollection flatCollect: [ :aPackage | SystemDependenciesReportResource current dependenciesReport dependenciesOf: aPackage ] as: Set.

	dependencies := dependencies copyWithoutAll: packagesCollection.

	^ dependencies sorted
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownBasicToolsDependencies [
	"ideally this list should be empty"

	^ #( 'Athens-Cairo' 'Athens-Core' #'Commander-Core' #Reflectivity #'Reflectivity-Tools' #Shout 'HeuristicCompletion-Model'
	     #VariablesLibrary #'Spec2-CommonWidgets' #'NewTools-Scopes' #'Traits-Tests' 'NewTools-Scopes' 'NewTools-Transcript' #'NewTools-FileBrowser' #EnlumineurFormatter )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownCompilerDependencies [
	"ideally this list should be empty"

	^ #( #'FileSystem-Core' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownDebuggingDependencies [
	"ideally this list should be empty"

	^ #( #'Morphic-Core' #'System-Object Events' #'Tool-Base' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownDisplayDependencies [
	"ideally this list should be empty"

	^ #( #'Morphic-Core' #'System-Object Events' #'Tool-Base' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownKernelAdditionalPackagesDependencies [
	"ideally this list should be empty"

	^ #( #'FileSystem-Core' 'CodeImport' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownKernelDependencies [
	"ideally this list should be empty"

	^ #( #'FileSystem-Core' 'CodeImport' 'Math-Operations-Extensions' 'System-Time' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownMorphicCoreDependencies [
	"ideally this list should be empty"

	^ #(#'Keymapping-KeyCombinations' #'Morphic-Base' 'Morphic-Widgets-ColorPicker' 'Tool-Base')
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownMorphicDependencies [
	"ideally this list should be empty"

	^ #(#'NewTools-FileBrowser' #'Tools-CodeNavigation' #EnlumineurFormatter ) "Rubric has a dependency on It"

]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownUIDependencies [
	"ideally this list should be empty"

	^ #( 'Athens-Cairo' 'Athens-Core' #'Refactoring-Environment' 'Reflectivity-Tools' #Shout #'Tool-Diff' #'HeuristicCompletion-Model'
	     'NECompletion-Morphic' #VariablesLibrary #'Tools-CodeNavigation' #'Spec2-CommonWidgets' #'NewTools-Scopes' #'NewTools-FileBrowser' #EnlumineurFormatter #'System-Changes' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownUIInfrastructureDependencies [
	"ideally this list should be empty"

	^ #( #'Morphic-Core' #'System-Object Events' #'Tool-Base' )
]

{ #category : 'accessing' }
SystemDependenciesTest >> packagesOfGroupNamed: aGroupName on: aBaseline [
	| allMembers allPackagesInBaseline packages groups |

	allMembers := aBaseline version groups
		detect: [ :g | g name = aGroupName ]
		ifNone: [ ^#() ].
	allPackagesInBaseline := aBaseline version packages collect: [:each | each name].
	packages := allMembers includes select: [ :aName | allPackagesInBaseline includes: aName ].
	groups := allMembers includes reject: [ :aName | allPackagesInBaseline includes: aName ].
	^ packages , (groups flatCollect: [:aInnerGroupName | self packagesOfGroupNamed: aInnerGroupName on: aBaseline ])
]

{ #category : 'tests' }
SystemDependenciesTest >> testCollectionsShouldNotDependOnRandom [
	"In the past Random-Core was depending on Collection-Abstract and Collections-Sequenceable but both those packages also depended on Random-Core.
	This dependency has been removed and this test is here to ensure it does not get added back in the future."

	| dependencies |
	dependencies := self externalDependendiesOf: #( 'Collections-Abstract' 'Collections-Sequenceable' ).
	self deny: (dependencies includes: 'Random-Core')
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalBasicToolsDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self _10debuggingLayerPackages,
		BaselineOfKernel testsPackageNames,
		BaselineOfCompiler testsPackageNames,
		BaselineOfFileSystem testsPackageNames,
		BaselineOfTraits coreTraitsPackageNames,
		BaselineOfSUnit withAllPackageNames,
		BaselineOfDisplay withAllPackageNames,
		BaselineOfUnifiedFFI withAllPackageNames,
		BaselineOfFonts allPackageNames,
		{ BaselineOfFreeType name }, (BaselineOfFreeType deepPackagesOfGroupNamed: #ui),
		BaselineOfKeymapping corePackageNames,
		BaselineOfMorphicCore withAllPackageNames,
		BaselineOfMorphic withAllPackageNames,
		BaselineOfMenuRegistration withAllPackageNames,
		BaselineOfUIInfrastructure withAllPackageNames,
		BaselineOfUI withAllPackageNames,
		BaselineOfRefactoring withAllPackageNames,
		{ BaselineOfCommander2 name }, (BaselineOfCommander2 deepPackagesOfGroupNamed: #core),
		{ BaselineOfCommander2 name }, (BaselineOfCommander2 deepPackagesOfGroupNamed: #ui),
		{ BaselineOfNewValueHolder name }, (BaselineOfNewValueHolder packagesOfGroupNamed: #core),
		{ BaselineOfSpecCore name }, (BaselineOfSpecCore deepPackagesOfGroupNamed: #default),
		{ BaselineOfSpec2 name }, (BaselineOfSpec2 deepPackagesOfGroupNamed: #default),
		BaselineOfBasicTools withAllPackageNames,
		{ BaselineOfNewTools name }, (BaselineOfNewTools deepPackagesOfGroupNamed: 'Methods'),
		(BaselineOfFuel deepPackagesOfGroupNamed: #Core),
		BaselineOfThreadedFFI corePackageNames ).

	self assertCollection: dependencies hasSameElements: self knownBasicToolsDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalCompilerDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf: self _03compilerLayerPackages.

	self assertCollection: dependencies hasSameElements: self knownCompilerDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalDebuggingDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf: self _10debuggingLayerPackages.

	self assertCollection: dependencies hasSameElements: self knownDebuggingDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalDisplayDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self _10debuggingLayerPackages, 
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfThreadedFFI allPackageNames,
		BaselineOfDisplay allPackageNames).

	self assert: dependencies equals: self knownDisplayDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalFileSystemDependencies [

	self assertEmpty: (self externalDependendiesOf: self _04fileSystemLayerPackages)
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalKernelAdditionalPackagesDependencies [

	self
		assertCollection: (self externalDependendiesOf: self _02kernelAdditionalPackagesLayerPackages)
		hasSameElements: self knownKernelAdditionalPackagesDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalKernelDependencies [
"
If you will break or weaken this test, a puppy will die!!!
     _ _      _
    o'')}____//
     `_/      )
     (_(_/-(_/
"

	self assertCollection: (self externalDependendiesOf: self _01kernelLayerPackages) hasSameElements: self knownKernelDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMetacelloCoreDependencies [

	self assertEmpty: (self externalDependendiesOf: self _07metacelloCoreLayerPackages)
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMinimalImageWithSUnitDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf: self _05traitsLayerPackages , BaselineOfPharoBootstrap sUnitPackageNames.

	self assertEmpty: dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMonticelloCoreDependencies [

	self assertEmpty: (self externalDependendiesOf: self _06monticelloCoreLayerPackages)
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMonticelloDependencies [

	self assertEmpty: (self externalDependendiesOf: self _08monticelloLayerPackages)
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMorphicCoreDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self _10debuggingLayerPackages,
		BaselineOfUIInfrastructure withAllPackageNames,
		BaselineOfMenuRegistration withAllPackageNames,
		BaselineOfTraits corePackageNames,
		BaselineOfSUnit corePackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfFonts allPackageNames,
		(BaselineOfFreeType deepPackagesOfGroupNamed: #ui),
		BaselineOfMorphicCore allPackageNames,
		BaselineOfThreadedFFI corePackageNames ).

	self assertCollection: dependencies hasSameElements: self knownMorphicCoreDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMorphicDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self _10debuggingLayerPackages,
		BaselineOfUIInfrastructure withAllPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfFonts allPackageNames,
		BaselineOfKeymapping corePackageNames,
		(BaselineOfFreeType deepPackagesOfGroupNamed: #ui),
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		BaselineOfMenuRegistration allPackageNames,
		(BaselineOfFuel deepPackagesOfGroupNamed: #Core),
		(BaselineOfNewValueHolder packagesOfGroupNamed: #core),
		BaselineOfThreadedFFI corePackageNames ).

	self assertCollection: dependencies hasSameElements: self knownMorphicDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalSUnitCoreDependencies [

	self assertEmpty: (self externalDependendiesOf: self _09sunitCoreLayerPackages)
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalSpec2Dependencies [

	| dependencies |

	dependencies := (self externalDependendiesOf: (
		"Language"
		self _10debuggingLayerPackages,
		BaselineOfUI allPackageNames,
		BaselineOfUIInfrastructure withAllPackageNames,
		BaselineOfTraits coreTraitsPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfFonts allPackageNames,

		"Morphic - for Morphic backend"
		BaselineOfDisplay allPackageNames,
		BaselineOfKeymapping corePackageNames,
		(BaselineOfFreeType deepPackagesOfGroupNamed: #ui),
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		BaselineOfMenuRegistration allPackageNames,
		(BaselineOfFuel deepPackagesOfGroupNamed: #Core),

		(BaselineOfCommander2 deepPackagesOfGroupNamed: #default),

		(BaselineOfNewValueHolder packagesOfGroupNamed: #core),
		(BaselineOfSpecCore deepPackagesOfGroupNamed: #default),
		(BaselineOfSpec2 deepPackagesOfGroupNamed: #default),
		BaselineOfThreadedFFI corePackageNames))
			copyWithoutAll: self knownMorphicDependencies, self knownBasicToolsDependencies, self knownUIDependencies.

	self assertEmpty: dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalTraitsDependencies [

	self assertEmpty: (self externalDependendiesOf: self _05traitsLayerPackages)
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalUFFIDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self _09sunitCoreLayerPackages,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfThreadedFFI corePackageNames).

	self assertEmpty: dependencies 
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalUIDependencies [

	| dependencies |

	dependencies := (self externalDependendiesOf: (
		self _10debuggingLayerPackages,
		BaselineOfUI allPackageNames,
		BaselineOfUIInfrastructure withAllPackageNames,
		BaselineOfTraits coreTraitsPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfFonts allPackageNames,
		BaselineOfKeymapping corePackageNames,
		(BaselineOfFreeType deepPackagesOfGroupNamed: #ui),
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		(BaselineOfCommander2 deepPackagesOfGroupNamed: #default),
		(BaselineOfNewValueHolder packagesOfGroupNamed: #core),
		(BaselineOfSpecCore deepPackagesOfGroupNamed: #default),
		(BaselineOfSpec2 deepPackagesOfGroupNamed: #default),
		(BaselineOfNewTools deepPackagesOfGroupNamed: 'Methods'),
		BaselineOfUI allPackageNames,
		BaselineOfMenuRegistration allPackageNames,
		(BaselineOfFuel deepPackagesOfGroupNamed: #Core),
		BaselineOfThreadedFFI corePackageNames )).

	self assertCollection: dependencies hasSameElements: self knownUIDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalUIInfrastructureDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf: self uiInfrastructureCoreLayerPackages.

	self assertCollection: dependencies hasSameElements: self knownUIInfrastructureDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testFilesShouldNotDependOnRandom [
	"In the past Files was depending on Random-Core only to print a random number in a file name.
	Now, there is no dependency between Files and Random-Core and if possible we would like to keep it this way in order to extract Random-Core from the KernelGroup of the Bootstrap (and maybe of the bootstrap in itself generally)"

	| dependencies |
	dependencies := self externalDependendiesOf: #( 'Files' ).
	self deny: (dependencies includes: 'Random-Core')
]

{ #category : 'tests' }
SystemDependenciesTest >> testNoDependencyToDeprecatedPackages [

	| dependencies |
	dependencies := self externalDependendiesOf: (self class packageOrganizer packageNames reject: [ :name | name includesSubstring: 'Deprecated' ]).

	self assertEmpty: dependencies
]

{ #category : 'packages' }
SystemDependenciesTest >> uiInfrastructureCoreLayerPackages [

	^ self _10debuggingLayerPackages , BaselineOfUnifiedFFI allPackageNames , BaselineOfThreadedFFI allPackageNames , BaselineOfDisplay allPackageNames
	  , BaselineOfFonts corePackageNames , BaselineOfFreeType uiPackageNames , BaselineOfMenuRegistration allPackageNames , BaselineOfKeymapping corePackageNames
	  , BaselineOfFuel corePackageNames
]
