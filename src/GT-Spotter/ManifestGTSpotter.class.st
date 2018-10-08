"
Includes the Spotter from GT Toolkit
"
Class {
	#name : #ManifestGTSpotter,
	#superclass : #PackageManifest,
	#category : #'GT-Spotter-Manifest'
}

{ #category : #'code-critics' }
ManifestGTSpotter class >> ruleRBCollectSelectNotUsedRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#GTSpotterStep #selectFirst #false)) #'2017-03-05T17:36:01.509623+01:00') )
]
