"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestMorphicWidgetsPluggable,
	#superclass : #PackageManifest,
	#category : #'Morphic-Widgets-Pluggable'
}

{ #category : #'code-critics' }
ManifestMorphicWidgetsPluggable class >> ruleEmptyExceptionHandlerRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#PluggableTextMorph #acceptBasic #false)) #'2016-07-01T16:07:25.941995+02:00') )
]

{ #category : #'code-critics' }
ManifestMorphicWidgetsPluggable class >> ruleSentNotImplementedRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#PluggableTextMorph #acceptBasic #false)) #'2016-07-01T16:07:25.953189+02:00') )
]
