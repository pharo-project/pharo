"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestUnifiedFFITests,
	#superclass : #PackageManifest,
	#category : #'UnifiedFFI-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestUnifiedFFITests class >> ruleGRTemporaryNeitherReadNorWrittenRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FFICalloutAPITest #testByteArrayToExternalAddress #false)) #'2019-07-05T11:54:43.174226+02:00') )
]

{ #category : #'code-critics' }
ManifestUnifiedFFITests class >> ruleRBLiteralArrayContainsCommaRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'UnifiedFFI-Tests')) #'2019-07-05T11:49:09.935226+02:00') )
]

{ #category : #'code-critics' }
ManifestUnifiedFFITests class >> ruleRBLiteralArrayContainsSuspiciousTrueFalseOrNilRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FFIFunctionParserTest #testParseFunction #false)) #'2019-07-05T12:04:26.431226+02:00') )
]

{ #category : #'code-critics' }
ManifestUnifiedFFITests class >> ruleRBLongMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FFICalloutTest #testBuildingFnSpec #false)) #'2019-07-05T11:56:50.062226+02:00') )
]

{ #category : #'code-critics' }
ManifestUnifiedFFITests class >> ruleRBRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FFICompilerPluginTest #testDecompilationOfFFIMethodShouldHaveNamedArgs #false)) #'2019-07-05T11:57:50.898226+02:00') )
]

{ #category : #'code-critics' }
ManifestUnifiedFFITests class >> ruleRBShouldntRaiseErrorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FFICalloutAPITest #testCallWithObjectCreation #false)) #'2019-07-05T11:52:11.496226+02:00') #(#(#RGMethodDefinition #(#FFICalloutAPITest #testCallWithObjectParameter #false)) #'2019-07-05T11:52:27.292226+02:00') #(#(#RGMethodDefinition #(#FFICalloutAPITest #testCallWithSelfParameter #false)) #'2019-07-05T11:52:42.022226+02:00') )
]

{ #category : #'code-critics' }
ManifestUnifiedFFITests class >> ruleRBTempsReadBeforeWrittenRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FFICalloutAPITest #testCallWithObjectCreation #false)) #'2019-07-05T11:52:01.493226+02:00') )
]

{ #category : #'code-critics' }
ManifestUnifiedFFITests class >> ruleRBUncommonMessageSendRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#FFIExternalEnumerationTest)) #'2019-07-05T12:00:45.523226+02:00') )
]
