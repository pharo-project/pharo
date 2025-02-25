"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestBeaconCore',
	#superclass : 'PackageManifest',
	#category : 'Beacon-Core-Manifest',
	#package : 'Beacon-Core',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestBeaconCore class >> ruleBadMessageRule2V1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#Beacon #loggersOf: #false)) #'2021-10-27T17:36:35.297975+00:00') )
]

{ #category : 'code-critics' }
ManifestBeaconCore class >> ruleClassNotReferencedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#TranscriptLogger)) #'2021-10-27T17:54:25.753664+00:00') #(#(#RGClassDefinition #(#CircularMemoryLogger)) #'2021-10-27T17:54:55.481727+00:00') )
]

{ #category : 'code-critics' }
ManifestBeaconCore class >> ruleCodeCruftLeftInMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#TranscriptLogger #nextPut: #false)) #'2021-10-27T17:50:25.160819+00:00') )
]

{ #category : 'code-critics' }
ManifestBeaconCore class >> ruleRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'SignalLogger class' #startSingleGlobalLogger #true)) #'2021-10-27T17:42:20.023221+00:00') )
]
