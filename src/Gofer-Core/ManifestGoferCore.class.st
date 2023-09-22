"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestGoferCore',
	#superclass : 'PackageManifest',
	#category : 'Gofer-Core-Manifest',
	#package : 'Gofer-Core',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestGoferCore class >> ruleCollectionMessagesToExternalObjectRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#GoferChanges #addReference: #false)) #'2021-07-23T19:30:50.698573+02:00') )
]
