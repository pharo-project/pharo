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
SystemDependenciesTest >> knownCompilerDependencies [

	"ideally this list should be empty"	

	^ #(#DeprecatedFileStream #'FileSystem-Core' #Monticello #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Ring-Core-Kernel' #'System-Changes' #'System-Localization')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownFileSystemDependencies [

	"ideally this list should be empty"	

	^ #(#Monticello #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'Ring-Core-Kernel' #'System-Changes' #'System-Localization' #'Zinc-Resource-Meta-Core')
]

{ #category : #'known dependencies' }
SystemDependenciesTest >> knownKernelDependencies [

	"ideally this list should be empty"	

	^ #(#CodeExport #CodeImportCommandLineHandlers #DeprecatedFileStream #'FileSystem-Core' #Monticello #'Multilingual-Encodings' #'Multilingual-Languages' #'Multilingual-TextConversion' #'OpalCompiler-Core' #'Ring-Core-Kernel' #'System-Changes' #'System-Localization')
]

{ #category : #utility }
SystemDependenciesTest >> longTestCase [

	self timeLimit: 60 seconds.

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
SystemDependenciesTest >> testExternalKernelDependencies [

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: BaselineOfPharoBootstrap kernelPackageNames.
	
	self assertCollection: dependencies equals: self knownKernelDependencies
]
