"
Core classes of Pharo Smalltalk including basic objects, types, exceptions, process, etc.
"
Class {
	#name : 'ManifestKernel',
	#superclass : 'PackageManifest',
	#category : 'Kernel-Manifest',
	#package : 'Kernel',
	#tag : 'Manifest'
}

{ #category : 'meta-data' }
ManifestKernel class >> dependencies [
	^ #(#'System-Announcements' #'Collections-Strings' #'Collections-Unordered' #'Collections-Streams' #Files #'System-Object Events' #'Collections-Abstract' #'Collections-Native' #'Collections-Support' #'System-Support' #'Multilingual-Encodings' #'Collections-Sequenceable' #'NewValueHolder-Core' #Compiler #'OpalCompiler-Core' #'System-Finalization' #Traits #'Collections-Weak' #UIManager)
]

{ #category : 'meta-data - dependency analyser' }
ManifestKernel class >> manuallyResolvedDependencies [

	^ #( #'Shift-ClassBuilder' )
]

{ #category : 'meta-data' }
ManifestKernel class >> packageName [
	^ #Kernel
]

{ #category : 'code-critics' }
ManifestKernel class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Delay class' #delaySchedulerClass: #true)) #'2016-12-07T10:58:09.403079+08:00') #(#(#RGMethodDefinition #(#'Character class' #constantNameFor: #true)) #'2019-01-28T20:56:52.112604+01:00') )
]

{ #category : 'code-critics' }
ManifestKernel class >> ruleRBRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Character class' #allByteCharacters #true)) #'2019-01-28T20:55:28.683846+01:00') )
]

{ #category : 'code-critics' }
ManifestKernel class >> ruleReDeadMethodV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Character class' #characterSet: #true)) #'2019-01-28T20:56:23.286894+01:00') )
]
