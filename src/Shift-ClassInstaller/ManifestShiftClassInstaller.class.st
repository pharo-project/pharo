"
ShiftClassInstaller (responsible of installing a class in the system) and related classes
"
Class {
	#name : #ManifestShiftClassInstaller,
	#superclass : #PackageManifest,
	#category : #'Shift-ClassInstaller-Manifest'
}

{ #category : #'code-critics' }
ManifestShiftClassInstaller class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ShiftClassInstaller #fixSlotScope: #false)) #'2019-07-25T11:05:26.260339+02:00') )
]
