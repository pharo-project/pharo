Class {
	#name : #ManifestFuel,
	#superclass : #PackageManifest,
	#category : #Fuel
}

{ #category : #'meta data' }
ManifestFuel class >> ruleConsistencyCheckRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#Float #serializeOn: #false)) #'2013-02-25T14:50:01.564000001+01:00') )
]

{ #category : #'meta data' }
ManifestFuel class >> ruleIfTrueBlocksRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#FLLightGeneralMapper #visitSubstitution:by:onRecursionDo: #false)) #'2013-02-25T14:50:01.650000001+01:00') )
]
