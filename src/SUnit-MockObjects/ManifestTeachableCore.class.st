"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestTeachableCore',
	#superclass : 'PackageManifest',
	#category : 'SUnit-MockObjects-Manifest',
	#package : 'SUnit-MockObjects',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestTeachableCore class >> ruleLiteralArrayContainsSuspiciousTrueFalseOrNilRuleV1FalsePositive [
	^ #(#(#(#RGMetaclassDefinition #(#'ManifestTeachableCore class' #ManifestTeachableCore)) #'2023-01-19T00:09:26.194869+01:00') )
]

{ #category : 'code-critics' }
ManifestTeachableCore class >> ruleSentNotImplementedRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Teachable class' #example #true)) #'2023-01-19T00:08:17.533291+01:00') )
]
