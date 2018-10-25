"
Core classes of Pharo Smalltalk including basic objects, types, exceptions, process, etc.
"
Class {
	#name : #ManifestKernel,
	#superclass : #PackageManifest,
	#category : #'Kernel-Manifest'
}

{ #category : #'meta-data' }
ManifestKernel class >> dependencies [
	^ #(#'System-Announcements' #'System-CommandLine' #'Collections-Strings' #'System-Localization' #'Slot-Core' #'Collections-Unordered' #'Collections-Streams' #Files #'System-Object Events' #'Collections-Abstract' #'Collections-Native' #'Collections-Support' #'System-Support' #'Multilingual-Encodings' #'Collections-Sequenceable' #'NewValueHolder-Core' #'System-Sources' #Compiler #'OpalCompiler-Core' #'System-Finalization' #Traits #'Collections-Weak' #UIManager)
]

{ #category : #'meta-data - dependency analyser' }
ManifestKernel class >> ignoredDependencies [
	^ #('Reflectivity' 'Regex-Core' 'System-VMEvents' 'System-Settings' 'FFI-Kernel' ) 
]

{ #category : #'meta-data - dependency analyser' }
ManifestKernel class >> manuallyResolvedDependencies [
	^ #(#'System-Settings' #'System-Sources' #'System-Platforms' #'Transcript-Core' #'Multilingual-TextConversion' #'AST-Core' #'Shift-ClassBuilder' #'Regex-Core' #'System-Localization' #'RPackage-Core' #'Zinc-Character-Encoding-Core' #Reflectivity #'Shift-ClassInstaller' #'FFI-Kernel')
]

{ #category : #'meta-data' }
ManifestKernel class >> packageName [
	^ #Kernel
]

{ #category : #'code-critics' }
ManifestKernel class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Delay class' #delaySchedulerClass: #true)) #'2016-12-07T10:58:09.403079+08:00') )
]
