Class {
	#name : 'BaselineOfCollectionsNative',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfCollectionsNative',
	#package : 'BaselineOfCollectionsNative'
}

{ #category : 'baselines' }
BaselineOfCollectionsNative >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Collections-Native';
				package: 'Collections-Native-Tests' with: [ spec requires: #( 'Collections-Native' ) ].

			spec
				group: 'Core' with: { 'Collections-Native' };
				group: 'Tests' with: { 'Collections-Native-Tests' } ]
]
