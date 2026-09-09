"
I am a Deprecated package because I am the old version of rules system.
The new version is Renraku
"
Class {
	#name : 'ManifestRefactoringCritics',
	#superclass : 'PackageManifest',
	#category : 'Refactoring-Critics-Manifest',
	#package : 'Refactoring-Critics',
	#tag : 'Manifest'
}

{ #category : 'deprecation' }
ManifestRefactoringCritics class >> isDeprecated [ 

	^ true
]
