versions
version100: spec
	<version: '1.0.0' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.0.0'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/07/2014 14:48'.
		
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AliakseiSyrel.61';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.21'.
		spec 
			project: 'GlamourCore' with: '3.0.4'. ].
