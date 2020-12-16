"
This test checks dependencies in the image and proves that there are no new unknown dependencies between packages.

It is not a standard unit test because to compute dependencies takes a lot of time. It uses cached version of the dependenices report.

"
Class {
	#name : #SystemDependenciesTest,
	#superclass : #TestCase,
	#classInstVars : [
		'dependenciesReport'
	],
	#category : #'System-DependenciesTests'
}

{ #category : #accessing }
SystemDependenciesTest class >> dependenciesReport [

	^ dependenciesReport ifNil: [ self rebuildDependenciesReport ]

]

{ #category : #accessing }
SystemDependenciesTest class >> rebuildDependenciesReport [

	dependenciesReport := DADependencyChecker new computeImageDependencies.
	^ dependenciesReport
]

{ #category : #accessing }
SystemDependenciesTest class >> resetDependenciesReport [
	<script>
	"self resetDependenciesReport"

	dependenciesReport := nil
]

{ #category : #accessing }
SystemDependenciesTest >> dependenciesReport [

	^ self class dependenciesReport

]

{ #category : #accessing }
SystemDependenciesTest >> externalDependendiesOf: packagesCollection [

	| dependencies | 
	
	dependencies := packagesCollection flatCollect: [ :aPackage | self dependenciesReport dependenciesOf: aPackage ] as: Set.
	
	dependencies := dependencies copyWithoutAll: packagesCollection.
	
	^ dependencies sorted
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownBasicToolsDependencies [

	"ideally this list should be empty"	
	self flag: #todo. "The Deprecated80 dependency should soon vanish. This is just the time we finish Spec migration."
	^ #('AST-Core-Tests' 'Athens-Cairo' 'Athens-Core' "Those are added for the development of Spec2 and should probably be removed"
	#'Athens-Morphic' #NautilusRefactoring #'Refactoring-Critics' #'Reflectivity-Tools' #Shout 'ParametrizedTests' 'Deprecated80')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownCompilerDependencies [

	"ideally this list should be empty"	

	^ #(#'FileSystem-Core')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownDisplayDependencies [

	"ideally this list should be empty"	

	^ #(#'Graphics-Canvas' #'Graphics-Files' #'Polymorph-Widgets')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownIDEDependencies [

	"ideally this list should be empty"	
	
	self flag: #todo. "The Deprecated80 dependency should soon vanish. This is just the time we finish Spec migration."
	^ #(Deprecated80)
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownKernelDependencies [

	"ideally this list should be empty"	

	^ #(#'FileSystem-Core')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownMorphicCoreDependencies [

	"ideally this list should be empty"	

	^ #(#'Keymapping-KeyCombinations' #'Morphic-Base' #'Morphic-Widgets-Windows' #'Refactoring-Critics' #'Refactoring-Environment' #'System-Clipboard')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownMorphicDependencies [
	"ideally this list should be empty"

	^ #(#'Fonts-Chooser' #RecentSubmissions #'Refactoring-Critics' #'Refactoring-Environment' #Shout #'Spec-Core' "Spec-Core should be removed later." #'Spec2-Core' #'Tool-Diff' #'Tool-FileList' #'Tool-Profilers'
	#'Athens-Morphic' 	"Rubric has a dependency on It" )
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownSUnitDependencies [

	"ideally this list should be empty"	

	^ #(#'Refactoring-Critics' #'Refactoring-Environment')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownSUnitKernelDependencies [

	"ideally this list should be empty"	

	^ #(#'FileSystem-Core' #'System-Changes' #'AST-Core')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownSpec2Dependencies [
	
	^ #(
		'WebBrowser-Core' "Spec's Link adapter"
		'Keymapping-Settings' "Spec's KeymapBrowser example tool"
		)
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownUFFIDependencies [

	"ideally this list should be empty"	

	^ #(#'Refactoring-Critics' #'Refactoring-Environment')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownUIDependencies [

	"ideally this list should be empty"	

	self flag: #todo. "The Deprecated80 dependency should soon vanish. This is just the time we finish Spec migration."
	^ #('AST-Core-Tests'  'Athens-Cairo' 'Athens-Core' "Those are added for the development of Spec2 and should probably be removed"
	#'Athens-Morphic' #RecentSubmissions #'Refactoring-Critics' #'Refactoring-Environment' #'Reflectivity-Tools' #Shout #'Tool-Diff' #'Tool-FileList' #'Tool-ProcessBrowser' #'Tool-Profilers' 'Deprecated80')
]

{ #category : #utilities }
SystemDependenciesTest >> longTestCase [

	self timeLimit: 60 seconds.

]

{ #category : #accessing }
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

{ #category : #accessing }
SystemDependenciesTest >> packagesOfGroupNamed: aGroupName on: aBaseline [ 

	| allMembers allPackagesInBaseline packages groups |
	allMembers := aBaseline version groups detect: [ :g | g name = aGroupName ].
	allPackagesInBaseline := aBaseline version packages collect: #name.
	
	packages := allMembers includes select: [ :aName | allPackagesInBaseline includes: aName ].
	groups := allMembers includes reject: [ :aName | allPackagesInBaseline includes: aName ].
		
	^ packages , (groups flatCollect: [:aInnerGroupName | self packagesOfGroupNamed: aInnerGroupName on: aBaseline ]).

]

{ #category : #tests }
SystemDependenciesTest >> testExternalBasicToolsDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
		{ BaselineOfSUnit name }, BaselineOfSUnit allPackageNames, "ALL"
		{ BaselineOfDisplay name }, BaselineOfDisplay allPackageNames,
		{ BaselineOfUnifiedFFI name }, BaselineOfUnifiedFFI allPackageNames,
		{ BaselineOfMorphicCore name }, BaselineOfMorphicCore allPackageNames,
		{ BaselineOfMorphic name }, BaselineOfMorphic allPackageNames,
		{ BaselineOfMenuRegistration name }, BaselineOfMenuRegistration allPackageNames,
		{ BaselineOfUI name }, BaselineOfUI allPackageNames,
		{ BaselineOfSpec name }, BaselineOfSpec allPackageNames,
		{ BaselineOfCommander2 name }, (BaselineOfCommander2 deepPackagesOfGroupNamed: #default),
		{ BaselineOfSpec2 name }, BaselineOfSpec2 allPackageNames,
		{ BaselineOfBasicTools name }, BaselineOfBasicTools allPackageNames,
		{ BaselineOfFuel name }, (BaselineOfFuel packagesOfGroupNamed: #Core),
		{ BaselineOfFuelPlatform name }, (BaselineOfFuelPlatform allPackageNames) ).
	
	self assertCollection: dependencies hasSameElements: self knownBasicToolsDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalCompilerDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap multilingualPackageNames,
		BaselineOfPharoBootstrap kernelAdditionalPackagesNames,				
		BaselineOfPharoBootstrap compilerPackageNames).
	
	self assertCollection: dependencies hasSameElements: self knownCompilerDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalDisplayDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,		
		BaselineOfDisplay allPackageNames).
	
	self assert: dependencies isEmpty.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalFileSystemDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap multilingualPackageNames,
		BaselineOfPharoBootstrap kernelAdditionalPackagesNames,		
		BaselineOfPharoBootstrap compilerPackageNames,
		BaselineOfPharoBootstrap fileSystemPackageNames).
	
	self assert: dependencies isEmpty.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalIDEDependencies [
	| dependencies packages |
	self longTestCase.
	packages := self metacelloPackageNames , self tonelCorePackageNames , { BaselineOfPharoBootstrap name. BaselineOfMonticello name. BaselineOfMetacello name}.

	{BaselineOfAthens.
	BaselineOfBasicTools.
	BaselineOfDisplay.
	BaselineOfFlashback.
	BaselineOfIDE.
	BaselineOfMenuRegistration.
	BaselineOfMorphic.
	BaselineOfMorphicCore.
	BaselineOfSlot.
	BaselineOfSpec.
	BaselineOfSpec2.
	BaselineOfParametrizedTests.
	BaselineOfSUnit.
	BaselineOfTraits.
	BaselineOfUI.
	BaselineOfUnifiedFFI.
	BaselineOfClassAnnotation.
	BaselineOfSystemCommands.
	BaselineOfClassParser.
	BaselineOfRefactoring2.
	BaselineOfReferenceFinder.
	BaselineOfSortFunctions.
	BaselineOfGeneralTests.
	BaselineOfMisc.
	BaselineOfEpicea.
	BaselineOfCatalog.
	BaselineOfRefactoring.
	BaselineOfOSWindow.
	BaselineOfQA.
	BaselineOfPharoGlamour.
	BaselineOfPharoGT.
	BaselineOfManifest.
	BaselineOfDependencyAnalyzer.
	BaselineOfQualityAssistant.
	BaselineOfReflectivity.
	BaselineOfFuzzyMatcher.
	BaselineOfZodiac.
	BaselineOfReflectionMirrors.
	BaselineOfGeneralHelp.
	BaselineOfShout.
	BaselineOfKernelTests.
	 } do: [ :baseline | packages := packages , {baseline name} , baseline allPackageNames ].

	packages := packages , { BaselineOfFuel name } , (BaselineOfFuel packagesOfGroupNamed: #Core).
	packages := packages , { BaselineOfFuelPlatform name } , BaselineOfFuelPlatform allPackageNames.

	packages := packages ,  { BaselineOfCommander name} , (self packagesOfGroupNamed: 'default' on: BaselineOfCommander).
	packages := packages ,  { BaselineOfCommander2 name} , (self packagesOfGroupNamed: 'default' on: BaselineOfCommander2).
	packages := packages , { BaselineOfIceberg name. BaselineOfLibGit name. BaselineOfCalypso name}.

	packages := packages ,  (self packagesOfGroupNamed: 'FullEnvironment' on: BaselineOfCalypso ).
	packages := packages ,  (self packagesOfGroupNamed: 'SystemBrowser' on: BaselineOfCalypso ).
	packages := packages ,  (self packagesOfGroupNamed: 'CoreBrowser' on: BaselineOfCalypso ).
	packages := packages ,  (self packagesOfGroupNamed: 'CoreEnvironment' on: BaselineOfCalypso ).
	packages := packages ,  #('Calypso-SystemTools-Core').
	packages := packages ,  (BaselineOfMetacello packagesOfGroupNamed: 'Tests').
	

	packages := packages ,  (self packagesOfGroupNamed: 'default' on: BaselineOfIceberg ).
	packages := packages ,  (self packagesOfGroupNamed: 'default' on: BaselineOfLibGit ).

	dependencies := self externalDependendiesOf: packages.
	self assertCollection: dependencies hasSameElements: self knownIDEDependencies
]

{ #category : #tests }
SystemDependenciesTest >> testExternalKernelDependencies [

"
If you will break or weaken this test, a puppy will die!!! 
     _ _      _
    o'')}____//
     `_/      )
     (_(_/-(_/
" 
	| dependencies |
	self longTestCase.

	dependencies := self
		externalDependendiesOf: BaselineOfPharoBootstrap kernelPackageNames , BaselineOfPharoBootstrap multilingualPackageNames, BaselineOfPharoBootstrap kernelAdditionalPackagesNames.

	self assertCollection: dependencies hasSameElements: self knownKernelDependencies
]

{ #category : #tests }
SystemDependenciesTest >> testExternalLocalMonticelloDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfTraits corePackages,	
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap compilerPackageNames,
		BaselineOfPharoBootstrap multilingualPackageNames,
		BaselineOfPharoBootstrap fileSystemPackageNames,
		BaselineOfPharoBootstrap kernelAdditionalPackagesNames,
		BaselineOfMonticello corePackageNames).
	
	self assert: dependencies isEmpty.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalMetacelloDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: self metacelloPackageNames, BaselineOfTraits corePackages.
	
	self assert: dependencies isEmpty
]

{ #category : #tests }
SystemDependenciesTest >> testExternalMonticelloDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfTraits corePackages,		
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap multilingualPackageNames,
		BaselineOfPharoBootstrap compilerPackageNames,
		BaselineOfPharoBootstrap fileSystemPackageNames,
		BaselineOfPharoBootstrap kernelAdditionalPackagesNames,
		BaselineOfMonticello corePackageNames,
		BaselineOfMonticello remoteRepositoriesPackageNames).
	
	self assert: dependencies isEmpty.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalMorphicCoreDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,			
		BaselineOfSUnit defaultPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfMorphicCore allPackageNames ).
	
	self assertCollection: dependencies hasSameElements: self knownMorphicCoreDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalMorphicDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		BaselineOfMenuRegistration allPackageNames,
		(BaselineOfFuel packagesOfGroupNamed: #Core),
		BaselineOfFuelPlatform allPackageNames ).
	
	self assertCollection: dependencies hasSameElements: self knownMorphicDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalSUnitDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfTraits corePackages,			
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfSUnit defaultPackageNames).
	
	self assertCollection: dependencies hasSameElements: self knownSUnitDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalSUnitKernelDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfTraits corePackages,
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap multilingualPackageNames,
		BaselineOfPharoBootstrap kernelAdditionalPackagesNames,
		BaselineOfPharoBootstrap sUnitPackageNames).
	
	self assertCollection: dependencies hasSameElements: self knownSUnitKernelDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalSpec2Dependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := (self externalDependendiesOf: (
		"Language"
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
		BaselineOfUnifiedFFI allPackageNames,

		"Morphic - for Morphic backend"
		BaselineOfDisplay allPackageNames,
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		BaselineOfMenuRegistration allPackageNames,
		(BaselineOfFuel packagesOfGroupNamed: #Core),
		BaselineOfFuelPlatform allPackageNames,

		(BaselineOfCommander2 deepPackagesOfGroupNamed: #default),
		
		BaselineOfSpec2 allPackageNames,
	
		"Tests"
		BaselineOfSUnit defaultPackageNames,
		BaselineOfParametrizedTests allPackageNames))
			copyWithoutAll: self knownMorphicDependencies, self knownBasicToolsDependencies, self knownUIDependencies.

	self assertCollection: dependencies hasSameElements: self knownSpec2Dependencies
]

{ #category : #tests }
SystemDependenciesTest >> testExternalUFFIDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,				
		BaselineOfSUnit defaultPackageNames,
		BaselineOfUnifiedFFI allPackageNames).
	
	self assertCollection: dependencies hasSameElements: self knownUFFIDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalUIDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := (self externalDependendiesOf: (
		self metacelloPackageNames,
		self tonelCorePackageNames,
		BaselineOfTraits corePackages,
		BaselineOfParametrizedTests allPackageNames,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		BaselineOfSpec allPackageNames,
		(BaselineOfCommander2 deepPackagesOfGroupNamed: #default),
		BaselineOfSpec2 allPackageNames,
		BaselineOfUI allPackageNames,
		BaselineOfMenuRegistration allPackageNames,
		(BaselineOfFuel packagesOfGroupNamed: #Core),
		BaselineOfFuelPlatform allPackageNames )).
	
	self assertCollection: dependencies hasSameElements: self knownUIDependencies.
]

{ #category : #accessing }
SystemDependenciesTest >> tonelCorePackageNames [

	^ { BaselineOfTonel name }, (BaselineOfTonel packagesOfGroupNamed: #core)
]
