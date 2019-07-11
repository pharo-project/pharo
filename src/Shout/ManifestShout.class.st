"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestShout,
	#superclass : #PackageManifest,
	#category : #'Shout-Manifest'
}

{ #category : #'code-critics' }
ManifestShout class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#SHRBTextStyler #unstyledTextFrom: #false)) #'2019-07-11T10:39:24.675084+02:00') )
]
