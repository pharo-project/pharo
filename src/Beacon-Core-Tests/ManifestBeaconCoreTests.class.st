"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestBeaconCoreTests,
	#superclass : #PackageManifest,
	#category : #'Beacon-Core-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestBeaconCoreTests class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#WrapperSignalTest #testAsBeaconSignalTransformsIntoWrapperSignal #false)) #'2019-09-16T11:41:03.590505+02:00') )
]
