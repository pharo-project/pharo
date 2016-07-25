versions
version126: spec
	<version: '1.2.6' imports: #('0.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'version 1.2.6'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '8/29/2015 13:47'.

		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-MerwanOuddane.94';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-MerwanOuddane.83';
			package: 'OSWindow-VM' with: 'OSWindow-VM-MerwanOuddane.4';
			package: 'OSWindow-SDL2-Examples' with: 'OSWindow-SDL2-Examples-MerwanOuddane.7' ].