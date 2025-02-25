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

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownBasicToolsDependencies [
	"ideally this list should be empty"

	^ #( 'Athens-Cairo' 'Athens-Core' #'Athens-Morphic'
	     #'Refactoring-Critics' #'Commander-Core' #Reflectivity
	     #'Reflectivity-Tools' #Shout 'HeuristicCompletion-Model'
	     #VariablesLibrary #'Spec2-CommonWidgets' #'NewTools-Scopes'
		  #'Traits-Tests' 'NewTools-Scopes'
		  #'NewTools-FileBrowser')
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownCompilerDependencies [
	"ideally this list should be empty"

	^ #( #'FileSystem-Core' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownKernelDependencies [
	"ideally this list should be empty"

	^ #( #'FileSystem-Core' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownLocalMonticelloDependencies [
	"ideally this list should be empty"

	"Remove Zinc-HTTP next line once Zinc-Resource-Meta-Core does not depend anymore on Zinc-HTTP."

	^ #( #'Zinc-HTTP' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownMorphicCoreDependencies [
	"ideally this list should be empty"

	^ #(#'Fonts-Infrastructure' #'FreeType' #'Keymapping-KeyCombinations' #'Morphic-Base' #'Refactoring-Critics' #'Refactoring-Environment' #'Rubric' 
	  'Morphic-Widgets-ColorPicker')
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownMorphicDependencies [
	"ideally this list should be empty"

	^ #( #'Refactoring-Critics' #'Refactoring-Environment'
	     #'Spec2-Core' #'NewTools-FileBrowser' #'Athens-Morphic'
	     #'Tools-CodeNavigation' 'Debugger-Oups' ) "Rubric has a dependency on It"
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownSUnitDependencies [
	"ideally this list should be empty"

	^ #( #'Refactoring-Critics' #'Refactoring-Environment' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownSpec2Dependencies [

	^ #(
		'WebBrowser-Core' "Spec's Link adapter"
		)
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownUFFIDependencies [
	"ideally this list should be empty"

	^ #( #'Refactoring-Critics' #'Refactoring-Environment' )
]

{ #category : 'known dependencies' }
SystemDependenciesTest >> knownUIDependencies [
	"ideally this list should be empty"

	^ #('Athens-Cairo' 'Athens-Core'
	#'Athens-Morphic' #'Refactoring-Critics' #'Refactoring-Environment' 'Reflectivity-Tools' 
	#Shout #'Tool-Diff' #'HeuristicCompletion-Model' 'NECompletion-Morphic' 
	#VariablesLibrary #'Tools-CodeNavigation' #'Spec2-CommonWidgets' 
	#'NewTools-Scopes' #'NewTools-FileBrowser')
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
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfKernelTests withAllPackageNames,
		BaselineOfTraits corePackages,
		BaselineOfSUnit withAllPackageNames, "ALL"
		BaselineOfDisplay withAllPackageNames,
		BaselineOfUnifiedFFI withAllPackageNames,
		{ BaselineOfFreeType name }, (BaselineOfFreeType deepPackagesOfGroupNamed: #ui),
		{ BaselineOfKeymapping name }, (BaselineOfKeymapping deepPackagesOfGroupNamed: #ui),
		BaselineOfMorphicCore withAllPackageNames,
		BaselineOfMorphic withAllPackageNames,
		BaselineOfMenuRegistration withAllPackageNames,
		BaselineOfUI withAllPackageNames,
		BaselineOfRefactoring withAllPackageNames,
		{ BaselineOfCommander2 name }, (BaselineOfCommander2 deepPackagesOfGroupNamed: #core),
		{ BaselineOfCommander2 name }, (BaselineOfCommander2 deepPackagesOfGroupNamed: #ui),
		{ BaselineOfNewValueHolder name }, (BaselineOfNewValueHolder packagesOfGroupNamed: #core),
		{ BaselineOfSpecCore name }, (BaselineOfSpecCore deepPackagesOfGroupNamed: #default),
		{ BaselineOfSpec2 name }, (BaselineOfSpec2 deepPackagesOfGroupNamed: #default),
		BaselineOfBasicTools withAllPackageNames,
		{ BaselineOfNewTools name }, (BaselineOfNewTools deepPackagesOfGroupNamed: 'Methods'),
		{ BaselineOfFuel name }, (BaselineOfFuel deepPackagesOfGroupNamed: #Tests),
		BaselineOfThreadedFFI corePackageNames ).

	self assertCollection: dependencies hasSameElements: self knownBasicToolsDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalCompilerDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap multilingualPackageNames,
		BaselineOfPharoBootstrap kernelAdditionalPackagesNames,
		BaselineOfPharoBootstrap compilerPackageNames).

	self assertCollection: dependencies hasSameElements: self knownCompilerDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalDisplayDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
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
SystemDependenciesTest >> testExternalLocalMonticelloDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf:
		                BaselineOfTraits corePackages , BaselineOfPharoBootstrap kernelPackageNames , BaselineOfPharoBootstrap compilerPackageNames
		                , BaselineOfPharoBootstrap multilingualPackageNames , BaselineOfPharoBootstrap fileSystemPackageNames
		                , BaselineOfPharoBootstrap kernelAdditionalPackagesNames , BaselineOfMonticello corePackageNames.

	self assertCollection: dependencies hasSameElements: self knownLocalMonticelloDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMetacelloDependencies [

	| dependencies |
	dependencies := self externalDependendiesOf: self metacelloPackageNames , BaselineOfTraits corePackages.

	self assertEmpty: dependencies 
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMonticelloDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		BaselineOfTraits corePackages,
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap multilingualPackageNames,
		BaselineOfPharoBootstrap compilerPackageNames,
		BaselineOfPharoBootstrap fileSystemPackageNames,
		BaselineOfPharoBootstrap kernelAdditionalPackagesNames,
		BaselineOfMonticello corePackageNames,
		BaselineOfMonticello remoteRepositoriesPackageNames).

	self assertEmpty: dependencies 
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalMorphicCoreDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
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
		BaselineOfTraits corePackages,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		(BaselineOfKeymapping deepPackagesOfGroupNamed: #morphic),
		(BaselineOfFreeType deepPackagesOfGroupNamed: #ui),
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		BaselineOfMenuRegistration allPackageNames,
		(BaselineOfFuel deepPackagesOfGroupNamed: #Tests),
		(BaselineOfNewValueHolder packagesOfGroupNamed: #core),
		BaselineOfThreadedFFI corePackageNames ).

	self assertCollection: dependencies hasSameElements: self knownMorphicDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalSUnitDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		BaselineOfTraits corePackages,
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfSUnit defaultPackageNames).

	self assertCollection: dependencies hasSameElements: self knownSUnitDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalSUnitKernelDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap compilerPackageNames,
		BaselineOfTraits corePackages,
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap multilingualPackageNames,
		BaselineOfPharoBootstrap kernelAdditionalPackagesNames,
		BaselineOfPharoBootstrap fileSystemPackageNames,
		BaselineOfPharoBootstrap sUnitPackageNames).

	self assertEmpty: dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalSpec2Dependencies [

	| dependencies |

	dependencies := (self externalDependendiesOf: (
		"Language"
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
		BaselineOfUnifiedFFI allPackageNames,

		"Morphic - for Morphic backend"
		BaselineOfDisplay allPackageNames,
		(BaselineOfKeymapping deepPackagesOfGroupNamed: #ui),
		(BaselineOfFreeType deepPackagesOfGroupNamed: #ui),
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		BaselineOfMenuRegistration allPackageNames,
		(BaselineOfFuel deepPackagesOfGroupNamed: #Tests),

		(BaselineOfCommander2 deepPackagesOfGroupNamed: #default),

		(BaselineOfNewValueHolder packagesOfGroupNamed: #core),
		(BaselineOfSpecCore deepPackagesOfGroupNamed: #default),
		(BaselineOfSpec2 deepPackagesOfGroupNamed: #default),
		BaselineOfThreadedFFI corePackageNames,

		"Tests"
		BaselineOfSUnit defaultPackageNames))
			copyWithoutAll: self knownMorphicDependencies, self knownBasicToolsDependencies, self knownUIDependencies.

	self assertCollection: dependencies hasSameElements: self knownSpec2Dependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalUFFIDependencies [

	| dependencies |

	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfThreadedFFI corePackageNames).

	self assertCollection: dependencies hasSameElements: self knownUFFIDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testExternalUIDependencies [

	| dependencies |

	dependencies := (self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		(BaselineOfKeymapping deepPackagesOfGroupNamed: #ui),
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
		(BaselineOfFuel deepPackagesOfGroupNamed: #Tests),
		BaselineOfThreadedFFI corePackageNames )).

	self assertCollection: dependencies hasSameElements: self knownUIDependencies
]

{ #category : 'tests' }
SystemDependenciesTest >> testFilesShouldNotDependOnRandom [
	"In the past Files was depending on Random-Core only to print a random number in a file name.
	Now, there is no dependency between Files and Random-Core and if possible we would like to keep it this way in order to extract Random-Core from the KernelGroup of the Bootstrap (and maybe of the bootstrap in itself generally)"

	| dependencies |
	dependencies := self externalDependendiesOf: #( 'Files' ).
	self deny: (dependencies includes: 'Random-Core')
]

{ #category : 'accessing' }
SystemDependenciesTest >> tonelCorePackageNames [

	^ { BaselineOfTonel name }, (BaselineOfTonel packagesOfGroupNamed: #core)
]
