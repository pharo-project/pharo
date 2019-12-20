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

	spec
		for: #common
		do: [ spec
				baseline: 'SUnitCore'
				with: [ spec
						loads: 'Core';
						className: 'BaselineOfSUnit';
						repository: repository ].

			spec
				baseline: 'Slot'
				with: [ spec
						loads: 'core';
						repository: repository ].

			spec
				baseline: 'Beacon'
				with: [ spec
						loads: #('CoreTests');
						repository: repository ].

			spec 
				baseline: 'Clap' 
				with: [ spec
					loads: #('core'); 
					repository: repository ] ]
]
