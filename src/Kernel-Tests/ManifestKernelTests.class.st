"
I contain tests for the Kernel package.
"
Class {
	#name : 'ManifestKernelTests',
	#superclass : 'PackageManifest',
	#category : 'Kernel-Tests-Manifest',
	#package : 'Kernel-Tests',
	#tag : 'Manifest'
}

{ #category : 'metadata' }
ManifestKernelTests class >> dependencies [

	^ #( #'Graphics-Primitives' #'System-Announcements' #'Morphic-Core' #'Morphic-Base' #'System-Changes' #'Collections-Strings' #'Collections-Unordered'
	   #Jobs #'Collections-Streams' #'FileSystem-Disk' #'System-Support' #'Text-Core' #'Collections-Abstract' #CollectionsTests #'Collections-Support'
	     #'Collections-Sequenceable' #'SUnit-Core' #Tests #'OpalCompiler-Core' #Traits #Kernel #'Collections-Weak' #UIManager )
]

{ #category : 'meta-data - dependency analyser' }
ManifestKernelTests class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Transcript-NonInteractive')
]

{ #category : 'metadata' }
ManifestKernelTests class >> packageName [
	^ #KernelTests
]

{ #category : 'code-critics' }
ManifestKernelTests class >> ruleCodeCruftLeftInMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#CompiledCodeTest #testHasLiteral #false)) #'2026-06-01T11:00:05.559108+02:00') )
]

{ #category : 'code-critics' }
ManifestKernelTests class >> ruleDeadBlockRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#CompiledCodeTest #methodSendingDoitToSelfWithUnusedBloc #false)) #'2026-06-01T10:59:14.984788+02:00') )
]

{ #category : 'code-critics' }
ManifestKernelTests class >> ruleExtraBlockRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#CompiledCodeTest #methodForTest5 #false)) #'2026-06-01T10:59:41.970308+02:00') )
]

{ #category : 'code-critics' }
ManifestKernelTests class >> ruleRBModifiesCollectionRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ClassHierarchyTest #testSubclassInstVar #false)) #'2019-06-26T12:47:46.581134+02:00') )
]

{ #category : 'code-critics' }
ManifestKernelTests class >> ruleSelfSentNotImplementedRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#CompiledCodeTest #methodForTest2 #false)) #'2026-05-29T15:14:10.242922+02:00') #(#(#RGMethodDefinition #(#CompiledCodeTest #methodSendingDoitToSelf #false)) #'2026-06-01T10:27:35.445639+02:00') #(#(#RGMethodDefinition #(#CompiledCodeTest #methodSendingDoitToSelfWithUnusedBloc #false)) #'2026-06-01T10:59:17.653294+02:00') #(#(#RGMethodDefinition #(#CompiledCodeTest #methodForTest4 #false)) #'2026-06-01T10:59:28.97238+02:00') #(#(#RGMethodDefinition #(#CompiledCodeTest #methodReturningBlock #false)) #'2026-06-01T10:59:51.16598+02:00') #(#(#RGMethodDefinition #(#CompiledCodeTest #methodForTest3 #false)) #'2026-06-01T10:59:58.627576+02:00') )
]

{ #category : 'code-critics' }
ManifestKernelTests class >> ruleSentNotImplementedRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#CompiledCodeTest #methodSendingDoitToSelf #false)) #'2026-06-01T10:27:26.457047+02:00') #(#(#RGMethodDefinition #(#CompiledCodeTest #methodSendingDoitToSelfWithUnusedBloc #false)) #'2026-06-01T10:59:12.206319+02:00') )
]
