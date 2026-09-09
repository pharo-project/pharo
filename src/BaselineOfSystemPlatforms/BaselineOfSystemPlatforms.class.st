Class {
	#name : 'BaselineOfSystemPlatforms',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSystemPlatforms',
	#package : 'BaselineOfSystemPlatforms'
}

{ #category : 'baselines' }
BaselineOfSystemPlatforms >> baseline: spec [

    <baseline>

	 spec for: #common do: [
			spec
				package: 'System-Platforms';
				package: 'System-Platforms-Tests' with: [ spec requires: #( 'System-Platforms' ) ].

			spec
				group: 'Core' with: { 'System-Platforms' };
				group: 'Tests' with: { 'System-Platforms-Tests' } 
	]
]
