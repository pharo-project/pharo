Class {
	#name : 'BaselineOfEpicea',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfEpicea',
	#package : 'BaselineOfEpicea'
}

{ #category : 'baselines' }
BaselineOfEpicea >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Ombu';
				package: 'Epicea' with: [ spec requires: 'Ombu' ];
				package: 'Hiedra';
				package: 'Hiedra-Examples'.

			spec
				package: 'Ombu-Tests' with: [ spec requires: 'Ombu' ];
				package: 'Epicea-Tests' with: [ spec requires: 'Epicea' ];
				package: 'Hiedra-Tests' with: [ spec requires: 'Hiedra' ].

			spec
				group: #Browsers with: #( #'Hiedra-Examples' );
				group: #Tests with: #( 'Ombu-Tests' 'Epicea-Tests' 'Hiedra-Tests' ) ]
]
