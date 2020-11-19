"
I contain tests for the Kernel package.
"
Class {
	#name : #ManifestKernelTests,
	#superclass : #PackageManifest,
	#category : #'Kernel-Tests-Manifest'
}

{ #category : #metadata }
ManifestKernelTests class >> dependencies [
	^ #(#'Graphics-Primitives' #'RPackage-Core' #'System-Announcements' #'Morphic-Core' #'Morphic-Base' #'System-Changes' #'Collections-Strings' #'Collections-Unordered' #'Slot-Core' #Jobs #'Collections-Streams' #'FileSystem-Disk' #'System-Support' #'Text-Core' #'Collections-Abstract' #CollectionsTests #'Collections-Support' #'Collections-Sequenceable' #'SUnit-Core' #Tests #'OpalCompiler-Core' #Traits #Kernel #'Collections-Weak' #UIManager)
]

{ #category : #metadata }
ManifestKernelTests class >> packageName [
	^ #KernelTests
]

{ #category : #'code-critics' }
ManifestKernelTests class >> ruleRBModifiesCollectionRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ClassHierarchyTest #testSubclassInstVar #false)) #'2019-06-26T12:47:46.581134+02:00') )
]
