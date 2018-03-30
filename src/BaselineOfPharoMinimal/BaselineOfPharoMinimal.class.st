Class {
	#name : #BaselineOfPharoMinimal,
	#superclass : #BaselineOf,
	#category : #BaselineOfPharoMinimal
}

{ #category : #baseline }
BaselineOfPharoMinimal >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURL.

	spec for: #common do: [
		spec
			baseline: 'PharoBootstrap' with: [spec repository: repository];
			baseline: 'Traits' with: [spec repository: repository];
			baseline: 'Traits' with: [spec repository: repository];
			baseline: 'Monticello' with: [spec repository: repository];
			baseline: 'Metacello' with: [spec repository: repository]. ]
]
