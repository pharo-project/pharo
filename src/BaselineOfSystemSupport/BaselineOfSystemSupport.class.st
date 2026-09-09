Class {
	#name : 'BaselineOfSystemSupport',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSystemSupport',
	#package : 'BaselineOfSystemSupport'
}

{ #category : 'baselines' }
BaselineOfSystemSupport >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			spec
				baseline: 'Collections' with: [
						spec
							repository: repository;
							loads: #( 'Collections-Unordered-Tests' ) ];
				package: 'System-Support';
				package: 'System-Support-Tests' with: [ spec requires: #( 'System-Support' 'Collections') ].

			spec
				group: 'Core' with: { 'System-Support' };
				group: 'Tests' with: { 'System-Support-Tests' } ]
]
