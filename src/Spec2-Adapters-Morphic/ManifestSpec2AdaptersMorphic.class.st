"
Morphic specific spec adapters
"
Class {
	#name : #ManifestSpec2AdaptersMorphic,
	#superclass : #PackageManifest,
	#category : #'Spec2-Adapters-Morphic-Manifest'
}

{ #category : #'code-critics' }
ManifestSpec2AdaptersMorphic class >> ruleGTExampleNotDefinedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#SpAbstractMorphicAdapter)) #'2016-07-01T15:56:13.367433+02:00') )
]

{ #category : #'code-critics' }
ManifestSpec2AdaptersMorphic class >> ruleRBOverridesDeprecatedMethodRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#SpMorphicMenuItemAdapter #name #false)) #'2016-07-01T15:56:13.381999+02:00') )
]
