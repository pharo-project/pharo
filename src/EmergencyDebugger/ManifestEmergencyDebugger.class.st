"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestEmergencyDebugger,
	#superclass : #PackageManifest,
	#category : #'EmergencyDebugger-Manifest'
}

{ #category : #'code-critics' }
ManifestEmergencyDebugger class >> ruleReToDoRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#EDEmergencyDebugger #composeStackText #false)) #'2020-04-10T12:25:37.258447+02:00') )
]
