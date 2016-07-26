baselines
baseline03: spec
	<version: '0.3-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/estebanlm/FastTable/main/'.
		spec description: 'creating a group to load glamour-fasttable package'.
		spec 
			package: #'Morphic-Widgets-FastTable';
			package: 'Glamour-FastTable' with: [
				spec 
					repository: 'http://www.smalltalkhub.com/mc/Moose/Glamour/main';
					requires: #('default' ). ].
		spec 
			group: 'default' with: #(#'Morphic-Widgets-FastTable' );
			group: 'glamour' with: #('Glamour-FastTable' ). ].
