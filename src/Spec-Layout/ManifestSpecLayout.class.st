"
Layouts and layout actions for Spec UI framework
"
Class {
	#name : #ManifestSpecLayout,
	#superclass : #PackageManifest,
	#category : #'Spec-Layout-Manifest'
}

{ #category : #deprecation }
ManifestSpecLayout class >> isDeprecated [
	"Spec should be replaced by Spec 2"

	^ true
]

{ #category : #'code-critics' }
ManifestSpecLayout class >> ruleExcessiveArgumentsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#SpecLayout #add:top:bottom:left:right: #false)) #'2016-07-01T15:56:13.487439+02:00') #(#(#RGMethodDefinition #(#SpecLayout #add:withSpec:top:bottom:left:right: #false)) #'2016-07-01T15:56:13.487448+02:00') #(#(#RGMethodDefinition #(#SpecLayout #add:withSpec:origin:corner:offsetOrigin:offsetCorner: #false)) #'2016-07-01T15:56:13.487458+02:00') #(#(#RGMethodDefinition #(#SpecLayout #newColumn:origin:corner:offsetOrigin:offsetCorner: #false)) #'2016-07-01T15:56:13.487419+02:00') #(#(#RGMethodDefinition #(#SpecLayout #newRow:origin:corner:offsetOrigin:offsetCorner: #false)) #'2016-07-01T15:56:13.487404+02:00') #(#(#RGMethodDefinition #(#SpecLayout #add:origin:corner:offsetOrigin:offsetCorner: #false)) #'2016-07-01T15:56:13.487429+02:00') #(#(#RGMethodDefinition #(#SpecLayout #setOffsetsFor:top:left:bottom:right:borderWidth: #false)) #'2016-07-01T15:56:13.487382+02:00') )
]
