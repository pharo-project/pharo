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
SystemDependenciesTest >> compilerLayerPackages [

	^ self kernelLayerPackages , BaselineOfPharoBootstrap compilerPackageNames
]

{ #category : 'packages' }
SystemDependenciesTest >> debuggingLayerPackages [

	^ self sunitCoreLayerPackages , BaselineOfDebugging corePackageNames
]

{ #category : 'accessing' }
SystemDependenciesTest >> dependenciesReport [

	^ SystemDependenciesReportResource current dependenciesReport
]

{ #category : 'accessing' }
SystemDependenciesTest >> externalDependendiesOf: packagesCollection [

	| dependencies |

	dependencies := packagesCollection flatCollect: [ :aPackage | self dependenciesReport dependenciesOf: aPackage ] as: Set.

	dependencies := dependencies copyWithoutAll: packagesCollection.

	^ dependencies sorted
]

{ #category : 'packages' }
SystemDependenciesTest >> kernelLayerPackages [

	^ (BaselineOfPharoBootstrap kernelPackageNames , BaselineOfPharoBootstrap multilingualPackageNames , BaselineOfPharoBootstrap kernelAdditionalPackagesNames)
		  asSet
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
SystemDependenciesTest >> knownKernelDependencies [
	"ideally this list should be empty"

	^ #( #'FileSystem-Core' 'CodeImport' )
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
	     'NECompletion-Morphic' #VariablesLibrary #'Tools-CodeNavigation' #'Spec2-CommonWidgets' #'NewTools-Scopes' #'NewTools-FileBrowser' #EnlumineurFormatter )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownUIInfrastructureDependencies [
	"ideally this list should be empty"

	^ #( #'Morphic-Core' #'System-Object Events' #'Tool-Base' )
]

{ #category : 'packages' }
SystemDependenciesTest >> metacelloCoreLayerPackages [

	^ self monticelloCoreLayerPackages , (BaselineOfMetacello packagesOfGroupNamed: 'Core') , (BaselineOfTonel packagesOfGroupNamed: #core)
]

{ #category : 'accessing' }
SystemDependenciesTest >> metacelloPackageNames [

	^ BaselineOfPharoBootstrap kernelPackageNames,
	  BaselineOfPharoBootstrap compilerPackageNames,
	  BaselineOfPharoBootstrap fileSystemPackageNames,
	  BaselineOfPharoBootstrap multilingualPackageNames,
	  BaselineOfPharoBootstrap kernelAdditionalPackagesNames,
	  BaselineOfMonticello corePackageNames,
	  BaselineOfMonticello remoteRepositoriesPackageNames,
	  (BaselineOfMetacello packagesOfGroupNamed: 'Core')
]

{ #category : 'packages' }
SystemDependenciesTest >> monticelloCoreLayerPackages [

	^ self compilerLayerPackages , BaselineOfTraits corePackages , BaselineOfPharoBootstrap fileSystemPackageNames , BaselineOfMonticello corePackageNames
]

{ #category : 'packages' }
SystemDependenciesTest >> monticelloLayerPackages [

	^ self metacelloCoreLayerPackages , BaselineOfMonticello remoteRepositoriesPackageNames
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

{ #category : 'packages' }
SystemDependenciesTest >> sunitCoreLayerPackages [

	^ self monticelloLayerPackages , BaselineOfSUnit corePackageNames
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
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfKernelTests withAllPackageNames,
		BaselineOfTraits corePackages,
		BaselineOfSUnit withAllPackageNames,
		BaselineOfDebugging corePackageNames,
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
	dependencies := self externalDependendiesOf: self compilerLayerPackages.

	self assertCollection: dependencies hasSameElements: self knownCompilerDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalDebuggingDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf: self debuggingLayerPackages.

	self assertCollection: dependencies hasSameElements: self knownDebuggingDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalDisplayDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
		BaselineOfSUnit corePackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfThreadedFFI allPackageNames,
		BaselineOfDisplay allPackageNames).

	self assertEmpty: dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalFileSystemDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf:
		                BaselineOfPharoBootstrap kernelPackageNames , BaselineOfPharoBootstrap multilingualPackageNames
		                , BaselineOfPharoBootstrap kernelAdditionalPackagesNames , BaselineOfPharoBootstrap compilerPackageNames
		                , BaselineOfPharoBootstrap fileSystemPackageNames.

	self assertEmpty: dependencies
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
	| dependencies |

	dependencies := self
		externalDependendiesOf: BaselineOfPharoBootstrap kernelPackageNames , BaselineOfPharoBootstrap multilingualPackageNames, BaselineOfPharoBootstrap kernelAdditionalPackagesNames.

	self assertCollection: dependencies hasSameElements: self knownKernelDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMetacelloCoreDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf: self metacelloCoreLayerPackages.

	self assertEmpty: dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMinimalImageWithSUnitDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf:
		                self compilerLayerPackages , BaselineOfTraits corePackages , BaselineOfPharoBootstrap fileSystemPackageNames
		                , BaselineOfPharoBootstrap sUnitPackageNames.

	self assertEmpty: dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMonticelloCoreDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf: self monticelloCoreLayerPackages.

	self assertEmpty: dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMonticelloDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf: self monticelloLayerPackages.

	self assertEmpty: dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMorphicCoreDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfDebugging corePackageNames,
		BaselineOfUIInfrastructure withAllPackageNames,
		BaselineOfTraits corePackages,
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
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfDebugging corePackageNames,
		BaselineOfUIInfrastructure withAllPackageNames,
		BaselineOfTraits corePackages,
		BaselineOfSUnit corePackageNames,
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

	| dependencies |
	dependencies := self externalDependendiesOf: self sunitCoreLayerPackages.

	self assertEmpty: dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalSpec2Dependencies [

	| dependencies |

	dependencies := (self externalDependendiesOf: (
		"Language"
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfDebugging corePackageNames,
		BaselineOfUI allPackageNames,
		BaselineOfUIInfrastructure withAllPackageNames,
		BaselineOfTraits corePackages,
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
		BaselineOfThreadedFFI corePackageNames,

		BaselineOfSUnit corePackageNames))
			copyWithoutAll: self knownMorphicDependencies, self knownBasicToolsDependencies, self knownUIDependencies.

	self assertEmpty: dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalUFFIDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
		BaselineOfSUnit corePackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfThreadedFFI corePackageNames).

	self assertEmpty: dependencies 
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalUIDependencies [

	| dependencies |

	dependencies := (self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfDebugging corePackageNames,
		BaselineOfUI allPackageNames,
		BaselineOfUIInfrastructure withAllPackageNames,
		BaselineOfTraits corePackages,
		BaselineOfSUnit corePackageNames,
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

{ #category : 'accessing' }
SystemDependenciesTest >> tonelCorePackageNames [

	^ { BaselineOfTonel name }, (BaselineOfTonel packagesOfGroupNamed: #core)
]

{ #category : 'packages' }
SystemDependenciesTest >> uiInfrastructureCoreLayerPackages [

	^ self debuggingLayerPackages , BaselineOfUnifiedFFI allPackageNames , BaselineOfThreadedFFI allPackageNames , BaselineOfDisplay allPackageNames
	  , BaselineOfFonts corePackageNames , BaselineOfFreeType uiPackageNames , BaselineOfMenuRegistration allPackageNames , BaselineOfKeymapping corePackageNames
	  , BaselineOfFuel corePackageNames
]
