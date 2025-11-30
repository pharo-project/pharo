Class {
	#name : 'BaselineOfMetacello',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMetacello',
	#package : 'BaselineOfMetacello'
}

{ #category : 'baseline' }
BaselineOfMetacello >> baseline: spec [
	<baseline>

	spec for: #common do: [
		spec 
			package: 'Metacello-Base';
			package: 'Metacello-Core';
			package: 'MonticelloFileTree-Core';
			package: 'MonticelloFileTree-FileSystem-Utilities';
			package: 'Zinc-Resource-Meta-Core';
			package: 'STON-Core';
			package: 'Metacello-CommandLine'. 

		spec
			package: 'Metacello-TestsCore';
			package: 'Metacello-TestsMCCore';
			package: 'Metacello-TestsReference'.

		spec 
			group: 'Core' with: #('Metacello-Base' 'Metacello-Core' 'MonticelloFileTree-Core' 'MonticelloFileTree-FileSystem-Utilities' 'Zinc-Resource-Meta-Core' 'STON-Core' 'Metacello-CommandLine');
			group: 'Tests' with: #( 'Metacello-TestsCore' 'Metacello-TestsMCCore' 'Metacello-TestsReference');
			group: 'default' with: #('Core') ]
]
