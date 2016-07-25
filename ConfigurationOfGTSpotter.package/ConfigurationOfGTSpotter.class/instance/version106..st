versions
version106: spec
	<version: '1.0.6' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/08/2014 21:45'.
		spec project: 'GlamourCore' with: '3.0.9'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AliakseiSyrel.77';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.34'. ].
