"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestSystemCommandLineHandlerTests',
	#superclass : 'PackageManifest',
	#category : 'System-CommandLineHandler-Tests-Manifest',
	#package : 'System-CommandLineHandler-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestSystemCommandLineHandlerTests class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#STCommandLineHandlerTest)) #'2019-06-30T23:16:50.788113+02:00') )
]
