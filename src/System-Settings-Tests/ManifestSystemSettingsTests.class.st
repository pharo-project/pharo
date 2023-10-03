"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestSystemSettingsTests',
	#superclass : 'PackageManifest',
	#category : 'System-Settings-Tests-Manifest',
	#package : 'System-Settings-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestSystemSettingsTests class >> ruleConsistencyCheckRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#SystemSettingsPersistenceTest #testStoreSettingNodes #false)) #'2023-02-28T10:40:51.861355+01:00') )
]
