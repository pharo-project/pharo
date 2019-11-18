Class {
	#name : #BaselineOfPharo,
	#superclass : #BaselineOf,
	#category : #BaselineOfPharo
}

{ #category : #baseline }
BaselineOfPharo >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURL.

	spec for: #common do: [
		spec postLoadDoIt: #'postload:package:'.
		spec baseline: 'BaseLibraries' with: [spec repository: repository].
		
		spec baseline: 'IDE' with: [spec repository: repository].
		spec baseline: 'Calypso' with: [ 
			spec repository: repository; loads: #('IcebergSupport'). ].

		spec package: 'Deprecated80'.
		spec package: 'Gofer-UI'.
	]
]

{ #category : #baseline }
BaselineOfPharo >> postload: loader package: packageSpec [

	WelcomeHelp openForRelease
]
