"
Growl is a small Morph extension to display notifications on the screen.

Freely inspired from the MIT Snarl developed by Tony Garnock-Jones.
"
Class {
	#name : #ManifestGrowl,
	#superclass : #PackageManifest,
	#category : #'Growl-Manifest'
}

{ #category : #'code-critics' }
ManifestGrowl class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#GrowlMorph #activeGrowlMorphs #false)) #'2018-06-27T18:15:09.571924+02:00') )
]

{ #category : #'code-critics' }
ManifestGrowl class >> ruleRBRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#GrowlMorph #activeGrowlMorphs #false)) #'2018-06-27T18:15:02.892206+02:00') )
]
