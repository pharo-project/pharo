versions
version010: spec
	<version: '0.10' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 0.10'.
		spec author: 'GuillermoPolito'.
		spec timestamp: '2/9/2016 15:20'.
		spec 
			package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-GuillermoPolito.162';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-TudorGirba.56'. ].
