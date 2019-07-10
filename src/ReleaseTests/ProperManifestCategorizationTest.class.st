"
Check for proper Manifest categorization
"
Class {
	#name : #ProperManifestCategorizationTest,
	#superclass : #TestCase,
	#category : #'ReleaseTests-Categorization'
}

{ #category : #tests }
ProperManifestCategorizationTest >> testProperManifestCategorization [
	"If a package provides a PackageManifest subclass then this manifest class should be packaged 
	 with 'Manifest' tag similar to all other manifests."
	
	|violations|
	violations := OrderedCollection new.
	PackageManifest allSubclassesDo: [:each |
 	   ((each package classesForClassTag: 'Manifest') includes: each)
  	      ifFalse: [ violations add: each ]
	].

	self assert: violations isEmpty.
]
