"
A package from the bootstrap layer (minimal image) is depending on an external package.
"
Class {
	#name : #PharoBootstrapRule,
	#superclass : #ReAbstractRule,
	#classVars : [
		'DependencyChecker'
	],
	#category : #'Kernel-Rules'
}

{ #category : #testing }
PharoBootstrapRule class >> checksPackage [

	^ true
]

{ #category : #running }
PharoBootstrapRule >> basicCheck: aPackage [
	(self bootstrapPackages includes: aPackage packageName)
		ifFalse: [ ^ false "Do not check" ].
	^ self newExternalDependencyExistFor: aPackage
]

{ #category : #accessing }
PharoBootstrapRule >> bootstrapPackages [
	^ #(
	#'AST-Core'
	#'Announcements-Core'
	#CodeExport
	#CodeImport
	#CodeImportCommandLineHandlers
	#Collections
	#Compression
	#'FileSystem-Core'
	#'FileSystem-Disk'
	#Files
	#Jobs
	#Kernel
	#Monticello
	#'Multilingual-Encodings'
	#'Multilingual-Languages'
	#'Multilingual-TextConversion'
	#'Network-UUID'
	#'OpalCompiler-Core'
	#'Random-Core'
	#'RPackage-Core'
	#'Ring-Definitions-Containers'
	#'Ring-Definitions-Core'
	#'Ring-Definitions-Monticello'
	#'Ring-OldChunkImporter'
	#'Slot-Core'
	#'System-Announcements'
	#'System-BasicCommandLineHandler'
	#'System-Changes'
	#'System-CommandLine'
	#'System-CommandLineHandler'
	#'System-Finalization'
	#'System-Hashing'
	#'System-Localization'
	#'System-Model'
	#'System-Platforms'
	#'System-SessionManager'
	#'System-Sources'
	#'System-Support'
	#Traits
	#'Transcript-Core'
	#UIManager
	#'Collections-Abstract'
	#'Collections-Arithmetic'
	#'Collections-Native'
	#'Collections-Sequenceable'
	#'Collections-Stack'
	#'Collections-Streams'
	#'Collections-Strings'
	#'Collections-Support'
	#'Collections-Unordered'
	#'Collections-Weak'
	#'Zinc-Character-Encoding-Core'
	#'Zinc-Resource-Meta-Core'
	#'PharoBootstrap-Initialization'
	#'Transcript-NonInteractive'
	)

]

{ #category : #accessing }
PharoBootstrapRule >> collectionsPackages [
	^ (CollectionsAssembly parts collect: #key)
		reject: [ :each | each = #'Collections-Atomic' ]
]

{ #category : #helpers }
PharoBootstrapRule >> critiqueFor: aPackage [

	^ ArchitecturalCritique
		withAnchor: (self anchorFor: aPackage)
		by: self
]

{ #category : #private }
PharoBootstrapRule >> dependencyChecker [
	^ (DependencyChecker ifNil: [ DADependencyChecker ]) new
]

{ #category : #accessing }
PharoBootstrapRule >> group [

	^ 'Architectural'
]

{ #category : #accessing }
PharoBootstrapRule >> name [
	^ 'Illegal dependency of Bootstrap Layer'
]

{ #category : #'instance creation' }
PharoBootstrapRule >> newExternalDependencyExistFor: aPackage [
	"Check if there are new dependencies to a package outside this layer"
	| dependencies newDependencies |
	dependencies := self dependencyChecker dependenciesOf: aPackage packageName.
	newDependencies := dependencies difference: self bootstrapPackages.
	^ newDependencies notEmpty
]
