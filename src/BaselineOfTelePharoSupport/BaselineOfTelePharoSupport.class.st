"
This baseline is used to load the basic support needed for remote image browsing with TelePharo. It is not used during building of the standard image.
"
Class {
	#name : #BaselineOfTelePharoSupport,
	#superclass : #BaselineOf,
	#category : #BaselineOfTelePharoSupport
}

{ #category : #baseline }
BaselineOfTelePharoSupport >> baseline: spec [ 
    
	<baseline>

	| repository | 
	
	repository := self packageRepositoryURL.

	spec for: #common do: [
		self tonel: spec.

		spec baseline: 'SUnit' with: [spec repository: repository].

		spec package: 'SortFunctions-Core'.
		spec package: 'Tool-Registry'.
	].
]

{ #category : #baseline }
BaselineOfTelePharoSupport >> tonel: spec [
	spec
		baseline: 'Tonel' 
		with: [ spec repository: 'github://pharo-vcs/tonel:v1.0.13' ]
]
