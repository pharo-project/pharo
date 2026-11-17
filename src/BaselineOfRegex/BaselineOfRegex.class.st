Class {
	#name : 'BaselineOfRegex',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfRegex',
	#package : 'BaselineOfRegex'
}

{ #category : 'baselines' }
BaselineOfRegex >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Regex-Core';
				package: 'Regex-Core-Tests' with: [ spec requires: #( 'Regex-Core' ) ].

			spec
				group: 'Core' with: #( 'Regex-Core' );
				group: 'Tests' with: #( 'Regex-Core-Tests' ) ]
]
