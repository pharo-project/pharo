"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemBasicCommandLineHandlerTests,
	#superclass : #PackageManifest,
	#category : #'System-BasicCommandLineHandler-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestSystemBasicCommandLineHandlerTests class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#STCommandLineHandlerTest)) #'2019-06-30T23:16:50.788113+02:00') )
]
