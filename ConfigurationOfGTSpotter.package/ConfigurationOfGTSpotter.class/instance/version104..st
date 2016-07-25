versions
version104: spec
	<version: '1.0.4' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.0.4 for Pharo 3.0'.
		spec author: 'TudorGirba'.
		spec timestamp: '1/04/2014 11:10'.
		spec project: 'GlamourCore' with: '3.0.7'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AliakseiSyrel.75';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AliakseiSyrel.30'. ].
