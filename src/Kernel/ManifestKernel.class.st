"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestKernel,
	#superclass : #PackageManifest,
	#category : #Kernel
}

{ #category : #'meta-data' }
ManifestKernel class >> dependencies [
	^ #(#'System-Announcements' #'System-CommandLine' #'Collections-Strings' #'System-Localization' #Slot #'Collections-Unordered' #'Collections-Streams' #Files #'System-Object Events' #'Collections-Abstract' #'Collections-Native' #'Collections-Support' #'System-Support' #'Multilingual-Encodings' #'Collections-Sequenceable' #NewValueHolder #'System-Sources' #Compiler #'OpalCompiler-Core' #'System-Finalization' #Traits #'Collections-Weak' #UIManager)
]

{ #category : #'meta-data' }
ManifestKernel class >> description [
	^ 'Core classes of Pharo Smalltalk including basic objects, types, exceptions, process, etc.'
]

{ #category : #'meta-data - dependency analyser' }
ManifestKernel class >> ignoredDependencies [
	^ #('Reflectivity' 'Regex-Core' 'System-VMEvents' 'System-Settings' 'FFI-Kernel' ) 
]

{ #category : #'meta-data - dependency analyser' }
ManifestKernel class >> manuallyResolvedDependencies [
	^ #(#'Regex-Core' #'System-Settings' #'Zinc-Character-Encoding-Core' #'FFI-Kernel' #'Multilingual-TextConversion' #'System-Platforms' #'Transcript-Core' #'AST-Core' #'System-Sources' #Reflectivity #'OpalCompiler-Core' #'System-Localization')
]

{ #category : #'meta-data' }
ManifestKernel class >> packageName [
	^ #Kernel
]

{ #category : #'code-critics' }
ManifestKernel class >> ruleConsistencyCheckRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#DelayScheduler #restoreResumptionTimes #false)) #'2014-10-26T23:52:40.336226+08:00') #(#(#RGMethodDefinition #(#DelayScheduler #saveResumptionTimes #false)) #'2014-10-26T23:52:40.336226+08:00') )
]

{ #category : #'code-critics' }
ManifestKernel class >> ruleEqualsTrueRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#DelayScheduler #startUp #false)) #'2014-10-26T23:52:40.367323+08:00') )
]

{ #category : #'code-critics' }
ManifestKernel class >> ruleLongMethodsRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#DelayScheduler #handleTimerEvent: #false)) #'2014-10-26T23:52:40.351947+08:00') )
]

{ #category : #'code-critics' }
ManifestKernel class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Delay class' #delaySchedulerClass: #true)) #'2016-12-07T10:58:09.403079+08:00') )
]

{ #category : #'code-critics' }
ManifestKernel >> ruleConsistencyCheckRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#DelayScheduler #restoreResumptionTimes #false)) #'2015-02-10T00:34:25.546586+08:00') #(#(#RGMethodDefinition #(#DelayScheduler #schedule: #false)) #'2015-02-10T00:34:25.546586+08:00') #(#(#RGMethodDefinition #(#DelayScheduler #saveResumptionTimes #false)) #'2015-02-10T00:34:25.546586+08:00') )
]
