"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestAthensBalloon,
	#superclass : #PackageManifest,
	#category : #'Athens-Balloon-Manifest'
}

{ #category : #'code-critics' }
ManifestAthensBalloon class >> ruleUnaryAccessingMethodWithoutReturnRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#AthensBalloonImagePaint #repeat #false)) #'2021-06-21T23:01:31.97848+02:00') )
]
