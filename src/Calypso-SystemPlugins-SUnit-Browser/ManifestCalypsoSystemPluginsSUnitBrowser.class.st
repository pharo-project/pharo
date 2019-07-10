"
I am a plugin for Calypso browser.

I add tools and commands to Calypso related to SUnit.

For example I add commands to run tests or commands to generate and jump to tests.
"
Class {
	#name : #ManifestCalypsoSystemPluginsSUnitBrowser,
	#superclass : #PackageManifest,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Manifest'
}

{ #category : #'code-critics' }
ManifestCalypsoSystemPluginsSUnitBrowser class >> ruleRBTempsReadBeforeWrittenRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#TClyGenerateTestClass #testClassFor: #false)) #'2018-11-12T15:00:23.227476+01:00') )
]
