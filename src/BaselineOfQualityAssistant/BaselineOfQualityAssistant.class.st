Class {
	#name : 'BaselineOfQualityAssistant',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfQualityAssistant',
	#package : 'BaselineOfQualityAssistant'
}

{ #category : 'baselines' }
BaselineOfQualityAssistant >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec
			package: 'Renraku';
			package: 'Renraku-Tests' with: [ spec requires: #( 'Renraku' ) ];
			"for now the required packages are not listed because it is a mess
				actions depend on UI and should be moved to NewTools. 
				"
			package: 'Renraku-UI' with: [ spec requires: #( 'Renraku' ) ].
		spec
			group: 'Core' with: #( 'Renraku' 'Renraku-UI');
			group: 'Tests' with: #( 'Renraku-Tests' );
			group: 'default' with: #( 'Core' 'Tests' ) ]
]
