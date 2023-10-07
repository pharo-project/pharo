"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestSystemSupportTests',
	#superclass : 'PackageManifest',
	#category : 'System-Support-Tests-Manifest',
	#package : 'System-Support-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestSystemSupportTests class >> ruleRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'System-Support-Tests')) #'2020-11-13T09:08:21.683585+01:00') )
]

{ #category : 'code-critics' }
ManifestSystemSupportTests class >> ruleStringConcatenationRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#SystemNavigationTest #testIsMessageSentInSystemWithClassesActuallySendngTheMessage #false)) #'2020-11-13T09:41:27.659044+01:00') #(#(#RGMethodDefinition #(#SystemNavigationTest #testIsMessageSentInSystemWithTheSelectorInsideAnArray #false)) #'2020-11-13T09:41:36.301311+01:00') )
]
