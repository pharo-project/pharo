Class {
	#name : 'BaselineOfCollectionsAbstract',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfCollectionsAbstract',
	#package : 'BaselineOfCollectionsAbstract'
}

{ #category : 'baselines' }
BaselineOfCollectionsAbstract >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Collections-Abstract';
				package: 'Collections-Abstract-Tests' with: [ spec requires: #( 'Collections-Abstract' ) ].

			spec
				group: 'Core' with: { 'Collections-Abstract' };
				group: 'Tests' with: { 'Collections-Abstract-Tests' } ]
]
