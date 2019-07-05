"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestEmergencyEvaluator,
	#superclass : #PackageManifest,
	#category : #'EmergencyEvaluator-Manifest'
}

{ #category : #'code-critics' }
ManifestEmergencyEvaluator class >> ruleRBCascadedNextPutAllsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#Transcripter #readEvalPrint #false)) #'2019-07-05T18:06:15.862172+02:00') )
]
