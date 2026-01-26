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

{ #category : 'baseline' }
BaselineOfMetacello >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Metacello-Core';
				package: 'Metacello-CommandLine';
				package: 'Metacello-Tests'.

			spec
				group: 'Core' with: #( 'Metacello-Core' 'Metacello-CommandLine' );
				group: 'Tests' with: #( 'Metacello-Tests' );
				group: 'default' with: #( 'Core' ) ]
]
