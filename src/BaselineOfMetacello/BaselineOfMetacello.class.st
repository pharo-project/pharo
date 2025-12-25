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
			package: 'Metacello-Base';
			package: 'Metacello-Core';
			package: 'Metacello-CommandLine'. 

		spec
			package: 'Metacello-TestsCore';
			package: 'Metacello-TestsMCCore'.

		spec 
			group: 'Core' with: #('Metacello-Base' 'Metacello-Core' 'Metacello-CommandLine');
			group: 'Tests' with: #( 'Metacello-TestsCore' 'Metacello-TestsMCCore');
			group: 'default' with: #('Core') ]
]
