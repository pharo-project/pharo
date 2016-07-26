versions
version083: spec
	<version: '0.8.3' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'fix hover problem (case 17379)'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '1/14/2016 17:05'.
		spec 
			package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-HenrikNergaard.148';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-AndreiChis.50'. ].
