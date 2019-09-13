"
I am a package part of the Spec 2 project.

Spec is a framework in Pharo for describing user interfaces. It allows for the construction of a wide variety of UIs; from small windows with a few but- tons up to complex tools like a debugger. Indeed, multiple tools in Pharo are written in Spec, e.g., Iceberg, Change Sorter, Critics Browser, and the Pharo debugger.

I am implementing the spec layer between presenters and the morphic widgets. I will be used when the morphic backend of Spec is selected to build the Morphic widgets during the GUI creation.
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
