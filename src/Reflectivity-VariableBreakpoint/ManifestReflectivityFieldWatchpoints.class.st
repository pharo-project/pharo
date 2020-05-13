"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestReflectivityFieldWatchpoints,
	#superclass : #PackageManifest,
	#category : #'Reflectivity-VariableBreakpoint-Manifest'
}

{ #category : #'code-critics' }
ManifestReflectivityFieldWatchpoints class >> ruleRBLongMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#VariableBreakpointTest #testAccessStrategy #false)) #'2020-04-21T18:06:08.416221+02:00') )
]
