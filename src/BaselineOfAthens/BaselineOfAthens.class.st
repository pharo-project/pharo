"
Baseline managing the Athens project for the Pharo bootstrap.

Load type: 
- linear
"
Class {
	#name : #BaselineOfAthens,
	#superclass : #BaselineOf,
	#category : #'BaselineOfAthens-Baseline'
}

{ #category : #baseline }
BaselineOfAthens >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			spec postLoadDoIt: #'postload:package:'.

			"Inter packages dependencies are now added but it is missing some dependencies on other projects such as Morphic for Athens-Morphic"
			"Dependencies"
			self
				display: spec;
				unifiedFFI: spec.

			"Packages"
			spec
				package: 'Athens-Core' with: [ spec requires: 'Display' ];
				package: 'Athens-Text' with: [ spec requires: 'Athens-Core' ];
				package: 'Athens-Balloon' with: [ spec requires: #('Athens-Core' 'Athens-Text') ];
				package: 'Athens-Morphic' with: [ spec requires: #('Athens-Core' 'Athens-Text' 'Athens-Balloon' 'Athens-Cairo') ];
				package: 'Athens-Cairo' with: [ spec requires: #('Athens-Core' 'UnifiedFFI') ];
				package: 'Athens-Examples' with: [ spec requires: #('Athens-Morphic' 'Athens-Text' 'Athens-Balloon' 'Athens-Cairo') ];
				package: 'Athens-Cairo-Tests' with: [ spec requires: 'Athens-Cairo' ].

			"Groups"
			spec
				group: 'Core' with: #('Cairo-core' 'Athens-Text' 'Athens-Balloon' 'Athens-Morphic');
				group: 'Cairo-core' with: #('Athens-Core' 'Athens-Cairo');
				group: 'Examples' with: #('Athens-Examples');
				group: 'Tests' with: #('Athens-Cairo-Tests') ]
]

{ #category : #baseline }
BaselineOfAthens >> display: spec [
	spec baseline: 'Display' with: [ spec repository: self repository ]
]

{ #category : #baseline }
BaselineOfAthens >> postload: loader package: packageSpec [

	AthensCanvas initialize.
]

{ #category : #accessing }
BaselineOfAthens >> repository [
	^ self packageRepositoryURL
]

{ #category : #baseline }
BaselineOfAthens >> unifiedFFI: spec [
	spec
		baseline: 'UnifiedFFI'
		with: [ spec
				loads: 'minimal';
				repository: self repository ]
]
