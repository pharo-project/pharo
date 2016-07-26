versions
version123: spec
	<version: '1.2.3' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.3'.
		spec author: 'MerwanOuddane'.
		spec timestamp: '8/25/2015 11:31'.
		spec 
			project: 'Athens' with: '3.4';
			project: 'NBOpenGL' with: '3.1'.
		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-MerwanOuddane.90';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-MerwanOuddane.78';
			package: 'OSWindow-VM' with: 'OSWindow-VM-IgorStasenko.3';
			package: 'NBOpenGL-OSWindow' with: 'NBOpenGL-OSWindow-RonieSalgado.1';
			package: 'NBXLib-Core' with: 'NBXLib-Core-RonieSalgado.13';
			package: 'OSWindow-X11' with: 'OSWindow-X11';
			package: 'OSWindow-TUIO' with: 'OSWindow-TUIO';
			package: 'OSWindow-SDL2-Examples' with: 'OSWindow-SDL2-Examples-MerwanOuddane.6';
			package: 'OSWindow-Core-Extras' with: 'OSWindow-Core-Extras-MerwanOuddane.2'. ].
