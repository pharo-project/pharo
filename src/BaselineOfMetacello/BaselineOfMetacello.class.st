Class {
	#name : 'BaselineOfMetacello',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMetacello',
	#package : 'BaselineOfMetacello'
}

{ #category : 'accessing' }
BaselineOfMetacello class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'baselines' }
BaselineOfMetacello >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Metacello-Core';
				package: 'Metacello-CommandLine';
				package: 'Metacello-MVS' with: [ spec requires: #( 'Metacello-Core' ) ];
				package: 'Metacello-Tests';
				package: 'Metacello-MVS-Tests' with: [ spec requires: #( 'Metacello-MVS' 'Metacello-Tests' ) ].

			spec
				group: 'Core' with: #( 'Metacello-Core' 'Metacello-CommandLine' 'Metacello-MVS' );
				group: 'Tests' with: #( 'Metacello-Tests' 'Metacello-MVS-Tests' );
				group: 'default' with: #( 'Core' ) ]
]
