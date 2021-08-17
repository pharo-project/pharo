"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestKernelChronologyExtras,
	#superclass : #PackageManifest,
	#category : #'Kernel-Chronology-Extras-Manifest'
}

{ #category : #'code-critics' }
ManifestKernelChronologyExtras class >> ruleClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Time class' #dateAndTimeFromSeconds: #true)) #'2021-06-24T08:33:11.204551+02:00') )
]
