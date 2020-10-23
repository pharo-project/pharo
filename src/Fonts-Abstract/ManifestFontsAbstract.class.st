"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestFontsAbstract,
	#superclass : #PackageManifest,
	#category : #'Fonts-Abstract-Manifest'
}

{ #category : #'code-critics' }
ManifestFontsAbstract class >> ruleRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'AbstractFont class' #update: #true)) #'2020-10-23T18:10:25.07485+02:00') )
]
