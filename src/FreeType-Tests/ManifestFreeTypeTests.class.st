"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestFreeTypeTests',
	#superclass : 'PackageManifest',
	#category : 'FreeType-Tests-Manifest',
	#package : 'FreeType-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestFreeTypeTests class >> ruleLongMethodsRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#FreeTypeCacheTest)) #'2022-09-02T00:12:14.694417+02:00') )
]
