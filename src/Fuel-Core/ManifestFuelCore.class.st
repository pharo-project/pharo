"
The core package of Fuel - the Smalltalk object serializer
"
Class {
	#name : #ManifestFuelCore,
	#superclass : #PackageManifest,
	#category : #'Fuel-Core-Manifest'
}

{ #category : #'meta data' }
ManifestFuelCore class >> ruleConsistencyCheckRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#Float #serializeOn: #false)) #'2013-02-25T14:50:01.564000001+01:00') )
]

{ #category : #'meta data' }
ManifestFuelCore class >> ruleIfTrueBlocksRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#FLLightGeneralMapper #visitSubstitution:by:onRecursionDo: #false)) #'2013-02-25T14:50:01.650000001+01:00') )
]
