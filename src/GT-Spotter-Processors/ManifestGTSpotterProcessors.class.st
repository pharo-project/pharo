"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestGTSpotterProcessors,
	#superclass : #PackageManifest,
	#category : #'GT-Spotter-Processors-Manifest'
}

{ #category : #'code-critics' }
ManifestGTSpotterProcessors class >> ruleReClassNotReferencedRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'GT-Spotter-Processors')) #'2020-03-26T18:37:51.234976+01:00') )
]
