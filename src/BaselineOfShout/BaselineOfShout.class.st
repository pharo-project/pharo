Class {
	#name : #BaselineOfShout,
	#superclass : #BaselineOf,
	#category : #BaselineOfShout
}

{ #category : #baselines }
BaselineOfShout >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURL.
	spec for: #'common' do: [
		
		spec baseline: 'Traits' with: [ 
			spec
				loads: #( 'traits-ast' );
				repository: repository ].
					
		spec 
			package: 'Shout';
			package: 'Rubric-Styling';
			package: 'Shout-Tests'.
		spec 
			group: 'Core' with: #('Shout' 'Rubric-Styling');
			group: 'Tests' with: #('Shout-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
