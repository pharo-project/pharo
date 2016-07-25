versions
version125: spec
	<version: '1.2.5' imports: #('0.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.5'.
		spec author: 'MerwanOuddane'.
		spec timestamp: '8/26/2015 15:42'.
		spec 
			project: 'Athens' with: '3.4';
			project: 'NBOpenGL' with: '3.1'.
		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-MerwanOuddane.93';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-MerwanOuddane.82';
			package: 'OSWindow-VM' with: 'OSWindow-VM-MerwanOuddane.4';
			package: 'OSWindow-SDL2-Examples' with: 'OSWindow-SDL2-Examples-MerwanOuddane.7'. ].
