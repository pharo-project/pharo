Class {
	#name : 'BaselineOfShout',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfShout',
	#package : 'BaselineOfShout'
}

{ #category : 'baselines' }
BaselineOfShout >> baseline: spec [

	<baseline>

	spec for: #common do: [
		spec
			package: 'Shout';
			package: 'Rubric-Styling';
			package: 'Shout-Tests';

			group: 'Core' with: #( 'Shout' 'Rubric-Styling' );
			group: 'Tests' with: #( 'Shout-Tests' );
			group: 'default' with: #( 'Core' 'Tests' ) ]
]
