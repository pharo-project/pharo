Class {
	#name : #BaselineOfBaseLibraries,
	#superclass : #BaselineOf,
	#category : #BaselineOfBaseLibraries
}

{ #category : #baselines }
BaselineOfBaseLibraries >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURL.

	spec for: #common do: [
		spec postLoadDoIt: #'postload:package:'.
		
		spec baseline: 'Beacon' with: [spec 
													loads: #('CoreTests');
													repository: repository].
		
	]
]
