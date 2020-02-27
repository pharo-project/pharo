"
Morphic specific spec adapters
"
Class {
	#name : #ManifestSpecMorphicAdapters,
	#superclass : #PackageManifest,
	#category : #'Spec-MorphicAdapters-Manifest'
}

{ #category : #deprecation }
ManifestSpecMorphicAdapters class >> isDeprecated [
	"Spec should be replaced by Spec 2"

	^ true
]

{ #category : #'code-critics' }
ManifestSpecMorphicAdapters class >> ruleGTExampleNotDefinedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#AbstractMorphicAdapter)) #'2016-07-01T15:56:13.367433+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecMorphicAdapters class >> ruleRBOverridesDeprecatedMethodRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#MorphicMenuItemAdapter #name #false)) #'2016-07-01T15:56:13.381999+02:00') )
]
