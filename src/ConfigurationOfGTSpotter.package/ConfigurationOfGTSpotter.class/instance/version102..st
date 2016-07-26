versions
version102: spec
	<version: '1.0.2' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.0.2'.
		spec author: 'TudorGirba'.
		spec timestamp: '12/23/2014 14:10'.
		spec project: 'GlamourCore' with: '3.0.6'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.70';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AliakseiSyrel.30'. ].
