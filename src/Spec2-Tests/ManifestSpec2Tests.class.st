"
I am a package part of the Spec 2 project.

Spec is a framework in Pharo for describing user interfaces. It allows for the construction of a wide variety of UIs; from small windows with a few but- tons up to complex tools like a debugger. Indeed, multiple tools in Pharo are written in Spec, e.g., Iceberg, Change Sorter, Critics Browser, and the Pharo debugger.

I contain the tests of Spec2-Core. Those are the tests of Presenters that are independant of the backend used by the user.
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
