"
Package for SUnit tests related to Spec UI framework
"
Class {
	#name : #ManifestSpecTests,
	#superclass : #PackageManifest,
	#category : #'Spec-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestSpecTests class >> ruleRBSentNotImplementedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#UIThemeDecoratorTest)) #'2018-10-10T14:57:43.139752+02:00') #(#(#RGMethodDefinition #(#RadioButtonGroupPresenterTest #testRebuildWidget #false)) #'2018-10-10T14:59:58.802602+02:00') )
]
