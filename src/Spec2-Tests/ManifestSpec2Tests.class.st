"
Package for SUnit tests related to Spec UI framework
"
Class {
	#name : #ManifestSpec2Tests,
	#superclass : #PackageManifest,
	#category : #'Spec2-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestSpec2Tests class >> ruleRBSentNotImplementedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#SpUIThemeDecoratorTest)) #'2018-10-10T14:57:43.139752+02:00') #(#(#RGMethodDefinition #(#RadioButtonGroupPresenterTest #testRebuildWidget #false)) #'2018-10-10T14:59:58.802602+02:00') )
]
