"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestMetacelloMVS',
	#superclass : 'PackageManifest',
	#category : 'Metacello-MVS-Manifest',
	#package : 'Metacello-MVS',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestMetacelloMVS class >> ruleExcessiveArgumentsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#MetacelloMVSRecordedSource #repository:at:class:methods:readFrom:problems: #false)) #'2026-11-10T10:35:09.800145+01:00') )
]
