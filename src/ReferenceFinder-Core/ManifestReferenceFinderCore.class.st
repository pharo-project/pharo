"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestReferenceFinderCore,
	#superclass : #PackageManifest,
	#category : #'ReferenceFinder-Core-Manifest'
}

{ #category : #'code-critics' }
ManifestReferenceFinderCore class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ReferenceFinder #_object:instVarAt: #false)) #'2019-07-13T17:57:52.391956+02:00') )
]

{ #category : #'code-critics' }
ManifestReferenceFinderCore class >> ruleRBUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ReferenceFinder #_object:instVarAt: #false)) #'2019-07-13T17:57:56.103956+02:00') #(#(#RGMethodDefinition #(#ReferenceFinder #_objectClass: #false)) #'2019-07-13T17:58:01.445956+02:00') #(#(#RGMethodDefinition #(#ReferenceFinder #_objectSize: #false)) #'2019-07-13T17:58:07.452956+02:00') #(#(#RGMethodDefinition #(#ReferenceFinder #_object:at: #false)) #'2019-07-13T17:58:22.138956+02:00') )
]
