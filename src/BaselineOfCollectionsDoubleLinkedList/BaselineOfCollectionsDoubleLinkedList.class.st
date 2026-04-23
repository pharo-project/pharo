Class {
	#name : 'BaselineOfCollectionsDoubleLinkedList',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfCollectionsDoubleLinkedList',
	#package : 'BaselineOfCollectionsDoubleLinkedList'
}

{ #category : 'baselines' }
BaselineOfCollectionsDoubleLinkedList >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Collections-DoubleLinkedList';
				package: 'Collections-DoubleLinkedList-Tests' with: [ spec requires: #( 'Collections-DoubleLinkedList' ) ].

			spec
				group: 'Core' with: { 'Collections-DoubleLinkedList' };
				group: 'Tests' with: { 'Collections-DoubleLinkedList-Tests' } ]
]
