Class {
	#name : 'BaselineOfFiles',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfFiles',
	#package : 'BaselineOfFiles'
}

{ #category : 'baselines' }
BaselineOfFiles >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Files';
				package: 'Files-Tests' with: [ spec requires: #( 'Files' ) ].

			spec
				group: 'Core' with: { 'Files' };
				group: 'Tests' with: { 'Files-Tests' } ]
]
