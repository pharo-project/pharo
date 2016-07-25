versions
version105: spec
	<version: '1.0.5' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/07/2014 17:49'.
		spec project: 'GlamourCore' with: '3.0.8'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AliakseiSyrel.76';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AliakseiSyrel.30'. ].
