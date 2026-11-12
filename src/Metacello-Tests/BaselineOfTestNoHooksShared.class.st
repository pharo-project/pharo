Class {
	#name : 'BaselineOfTestNoHooksShared',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'baselines' }
BaselineOfTestNoHooksShared >> baseline: spec [
	<baseline>
	spec for: #common do: [
			spec
				package: 'NoHooksCore';
				package: 'NoHooksTests'.
			spec
				group: 'Core' with: #( 'NoHooksCore' );
				group: 'Tests' with: #( 'NoHooksTests' ) ]
]
