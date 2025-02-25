"
General support for the Pharo system like access to the VM or image
"
Class {
	#name : 'ManifestSystemSupport',
	#superclass : 'PackageManifest',
	#category : 'System-Support-Manifest',
	#package : 'System-Support',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemSupport class >> ignoredDependencies [
	^ #(#'System-Settings-Core' #'Graphics-Primitives')
]

{ #category : 'meta-data - dependency analyser' }
ManifestSystemSupport class >> manuallyResolvedDependencies [
	^ #(#'Collections-Support' #'System-Settings-Core' #'System-Platforms')
]

{ #category : 'code-critics' }
ManifestSystemSupport class >> ruleBadMessageRule2V1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#SystemEnvironment #poolUsers #false)) #'2021-11-02T17:56:27.52638+01:00') #(#(#RGMethodDefinition #(#SystemEnvironment #fillCaches #false)) #'2021-11-02T17:58:27.713429+01:00') )
]

{ #category : 'code-critics' }
ManifestSystemSupport class >> ruleClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Author class' #useAuthor:during: #true)) #'2020-06-15T21:10:09.714675+02:00') )
]

{ #category : 'code-critics' }
ManifestSystemSupport class >> ruleInconsistentMethodClassificationRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#SystemEnvironment #at:put: #false)) #'2021-11-02T18:04:16.398839+01:00') #(#(#RGMethodDefinition #(#SystemEnvironment #removeKey:ifAbsent: #false)) #'2021-11-02T18:06:16.506535+01:00') )
]

{ #category : 'code-critics' }
ManifestSystemSupport class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#SystemEnvironment #veryDeepCopyWith: #false)) #'2021-11-02T18:14:26.131262+01:00') )
]
