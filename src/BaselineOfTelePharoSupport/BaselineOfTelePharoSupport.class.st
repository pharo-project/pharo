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
		spec baseline: 'SUnit' with: [spec repository: repository].
		spec baseline: 'Display' with: [spec repository: repository].

		spec package: 'SortFunctions-Core'.
		spec package: 'Tool-Registry'.
	].
]
