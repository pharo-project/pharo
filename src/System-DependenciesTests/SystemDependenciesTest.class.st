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
	#category : #System-DependenciesTests
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

	^ #(#'Athens-Morphic' #Compiler #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #NautilusRefactoring #'Refactoring-Critics' #'Reflectivity-Tools' #Shout)
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownCompilerDependencies [

	"ideally this list should be empty"	

	^ #(#DeprecatedFileStream #'FileSystem-Core' #Monticello #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Ring-Core-Kernel' #'System-Changes' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownDisplayDependencies [

	"ideally this list should be empty"	

	^ #(#'Fonts-Abstract' #'Graphics-Canvas' #'Graphics-Files' #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Network-Mail' #'Polymorph-Widgets' #'SUnit-Core' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownFileSystemDependencies [

	"ideally this list should be empty"	

	^ #(#Monticello #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Ring-Core-Kernel' #'System-Changes' #'System-Localization' #'Zinc-Resource-Meta-Core')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownIDEDependencies [

	"ideally this list should be empty"	

	^ #(#BaselineOfBasicTools #BaselineOfDisplay #BaselineOfIDE #BaselineOfMorphic #BaselineOfMorphicCore #BaselineOfSUnit #BaselineOfUI #BaselineOfUnifiedFFI #Compiler #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion').
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownKernelDependencies [

	"ideally this list should be empty"	

	^ #(#CodeExport #CodeImportCommandLineHandlers #DeprecatedFileStream #'FileSystem-Core' #Monticello #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'OpalCompiler-Core' #'Ring-Core-Kernel' #'System-Changes' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownLocalMonticelloDependencies [

	"ideally this list should be empty"	

	^ #(#'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownMetacelloDependencies [

	"ideally this list should be empty"	

	^ #(#'Fonts-Abstract' #'Graphics-Files' #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Network-Mail' #'SUnit-Core' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownMonticelloDependencies [

	"ideally this list should be empty"	

	^ #(#'Graphics-Files' #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Network-Mail' #'SUnit-Core' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownMorphicCoreDependencies [

	"ideally this list should be empty"	

	^ #(#Compiler #'Fonts-Abstract' #'Graphics-Files' #'Keymapping-KeyCombinations' #'Morphic-Base' #'Morphic-Widgets-Windows' #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Network-Mail' #'Polymorph-Widgets' #'Refactoring-Critics' #'Refactoring-Environment' #'System-Clipboard' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownMorphicDependencies [

	"ideally this list should be empty"	

	^ #(#Compiler #FontChooser #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Network-Mail' #RecentSubmissions #'Refactoring-Critics' #'Refactoring-Environment' #'Reflectivity-Tools' #Rubric #Shout #'Spec-Core' #'Tool-Base' #'Tool-Diff' #'Tool-FileList' #'Tool-Profilers')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownSUnitDependencies [

	"ideally this list should be empty"	

	^ #(#'Fonts-Abstract' #'Graphics-Files' #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Network-Mail' #'Refactoring-Critics' #'Refactoring-Environment' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownSUnitKernelDependencies [

	"ideally this list should be empty"	

	^ #(#CodeExport #CodeImportCommandLineHandlers #DeprecatedFileStream #'FileSystem-Core' #Monticello #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'OpalCompiler-Core' #'Ring-Core-Kernel' #'System-Changes' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownUFFIDependencies [

	"ideally this list should be empty"	

	^ #(#Compiler #'Fonts-Abstract' #'Graphics-Files' #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Network-Mail' #'Refactoring-Critics' #'Refactoring-Environment' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownUIDependencies [

	"ideally this list should be empty"	

	^ #(#'Athens-Morphic' #Compiler #Komitter #MonticelloGUI #'Morphic-Widgets-FastTable' #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #NautilusCommon #NautilusRefactoring #'Network-Mail' #RecentSubmissions #'Refactoring-Critics' #'Refactoring-Environment' #'Reflectivity-Tools' #Shout #'Tool-Diff' #'Tool-FileList' #'Tool-ProcessBrowser' #'Tool-Profilers')
]

{ #category : #utility }
SystemDependenciesTest >> longTestCase [

	self timeLimit: 60 seconds.

]

{ #category : #accessing }
SystemDependenciesTest >> metacelloPackageNames [

	^ BaselineOfPharoBootstrap kernelPackageNames,
	  BaselineOfPharoBootstrap compilerPackageNames,
	  BaselineOfPharoBootstrap fileSystemPackageNames,
	  BaselineOfMonticello corePackageNames,
	  BaselineOfMonticello remoteRepositoriesPackageNames,
	  BaselineOfMetacello allPackageNames
]

{ #category : #tests }
SystemDependenciesTest >> testExternalBasicToolsDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		BaselineOfUI allPackageNames,
		BaselineOfBasicTools allPackageNames ).
	
	self assertCollection: dependencies equals: self knownBasicToolsDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalCompilerDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap compilerPackageNames).
	
	self assertCollection: dependencies equals: self knownCompilerDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalDisplayDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		BaselineOfDisplay allPackageNames).
	
	self assertCollection: dependencies equals: self knownDisplayDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalFileSystemDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap compilerPackageNames,
		BaselineOfPharoBootstrap fileSystemPackageNames).
	
	self assertCollection: dependencies equals: self knownFileSystemDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalIDEDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		BaselineOfSUnit allPackageNames, "ALL"
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		BaselineOfUI allPackageNames,
		BaselineOfBasicTools allPackageNames,
		BaselineOfIDE allPackageNames ).
	
	self assertCollection: dependencies equals: self knownIDEDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalKernelDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: BaselineOfPharoBootstrap kernelPackageNames.
	
	self assertCollection: dependencies equals: self knownKernelDependencies
]

{ #category : #tests }
SystemDependenciesTest >> testExternalLocalMonticelloDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap compilerPackageNames,
		BaselineOfPharoBootstrap fileSystemPackageNames,
		BaselineOfMonticello corePackageNames).
	
	self assertCollection: dependencies equals: self knownLocalMonticelloDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalMetacelloDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: self metacelloPackageNames.
	
	self assertCollection: dependencies equals: self knownMetacelloDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalMonticelloDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap compilerPackageNames,
		BaselineOfPharoBootstrap fileSystemPackageNames,
		BaselineOfMonticello corePackageNames,
		BaselineOfMonticello remoteRepositoriesPackageNames).
	
	self assertCollection: dependencies equals: self knownMonticelloDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalMorphicCoreDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfMorphicCore allPackageNames ).
	
	self assertCollection: dependencies equals: self knownMorphicCoreDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalMorphicDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames ).
	
	self assertCollection: dependencies equals: self knownMorphicDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalSUnitDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		BaselineOfSUnit defaultPackageNames).
	
	self assertCollection: dependencies equals: self knownSUnitDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalSUnitKernelDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap sUnitPackageNames).
	
	self assertCollection: dependencies equals: self knownSUnitKernelDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalUFFIDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfUnifiedFFI allPackageNames).
	
	self assertCollection: dependencies equals: self knownUFFIDependencies.
]

{ #category : #tests }
SystemDependenciesTest >> testExternalUIDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		self metacelloPackageNames,
		BaselineOfSUnit defaultPackageNames,
		BaselineOfDisplay allPackageNames,
		BaselineOfUnifiedFFI allPackageNames,
		BaselineOfMorphicCore allPackageNames,
		BaselineOfMorphic allPackageNames,
		BaselineOfUI allPackageNames ).
	
	self assertCollection: dependencies equals: self knownUIDependencies.
]
