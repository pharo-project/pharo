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
	^ #(#'System-Announcements' #'System-CommandLine' #'Collections-Strings' #'Slot-Core' #'Collections-Unordered' #'Collections-Streams' #Files #'System-Object Events' #'Collections-Abstract' #'Collections-Native' #'Collections-Support' #'System-Support' #'Multilingual-Encodings' #'Collections-Sequenceable' #'NewValueHolder-Core' #'System-Sources' #Compiler #'OpalCompiler-Core' #'System-Finalization' #Traits #'Collections-Weak' #UIManager)
]

{ #category : #'meta-data - dependency analyser' }
ManifestKernel class >> ignoredDependencies [
	^ #('Reflectivity' 'Regex-Core' 'System-VMEvents' 'System-Settings-Core' 'FFI-Kernel' ) 
]

{ #category : #'meta-data - dependency analyser' }
ManifestKernel class >> manuallyResolvedDependencies [
	^ #(#'System-Settings-Core' #'System-Sources' #'System-Platforms' #'Multilingual-TextConversion' #'Shift-ClassBuilder' #'Regex-Core' #'RPackage-Core' #'Zinc-Character-Encoding-Core' #Reflectivity #'Shift-ClassInstaller' #'FFI-Kernel')
]

{ #category : #'meta-data' }
ManifestKernel class >> packageName [
	^ #Kernel
]

{ #category : #'code-critics' }
ManifestKernel class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Delay class' #delaySchedulerClass: #true)) #'2016-12-07T10:58:09.403079+08:00') #(#(#RGMethodDefinition #(#'Character class' #constantNameFor: #true)) #'2019-01-28T20:56:52.112604+01:00') )
]

{ #category : #'code-critics' }
ManifestKernel class >> ruleRBClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Character class' #allCharacters #true)) #'2019-01-28T20:55:17.784869+01:00') #(#(#RGMethodDefinition #(#'Character class' #allByteCharacters #true)) #'2019-01-28T20:55:32.378368+01:00') )
]

{ #category : #'code-critics' }
ManifestKernel class >> ruleRBRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Character class' #allByteCharacters #true)) #'2019-01-28T20:55:28.683846+01:00') )
]

{ #category : #'code-critics' }
ManifestKernel class >> ruleReDeadMethodV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Character class' #characterSet: #true)) #'2019-01-28T20:56:23.286894+01:00') )
]
