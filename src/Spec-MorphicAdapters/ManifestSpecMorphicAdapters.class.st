"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSpecMorphicAdapters,
	#superclass : #PackageManifest,
	#category : #'Spec-MorphicAdapters'
}

{ #category : #'code-critics' }
ManifestSpecMorphicAdapters class >> ruleGTExampleNotDefinedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#AbstractMorphicAdapter)) #'2016-07-01T15:56:13.367433+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecMorphicAdapters class >> ruleRBOverridesDeprecatedMethodRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#MorphicMenuItemAdapter #name #false)) #'2016-07-01T15:56:13.381999+02:00') )
]
