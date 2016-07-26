versions
version101: spec
	<version: '1.0.1' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.0.1'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/10/2014 13:38'.
		
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AliakseiSyrel.64';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.22'.
		spec 
			project: 'GlamourCore' with: '3.0.5'. ].
