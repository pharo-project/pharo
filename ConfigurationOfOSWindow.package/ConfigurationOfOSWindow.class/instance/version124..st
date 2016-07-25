versions
version124: spec
	<version: '1.2.4' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.4'.
		spec author: 'MerwanOuddane'.
		spec timestamp: '8/25/2015 15:53'.
		spec 
			project: 'Athens' with: '3.4';
			project: 'NBOpenGL' with: '3.1'.
		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-MerwanOuddane.92';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-MerwanOuddane.81';
			package: 'OSWindow-VM' with: 'OSWindow-VM-IgorStasenko.3';
			package: 'NBOpenGL-OSWindow' with: 'NBOpenGL-OSWindow';
			package: 'OSWindow-SDL2-OpenGL' with: 'OSWindow-SDL2-OpenGL-MerwanOuddane.2';
			package: 'NBXLib-Core' with: 'NBXLib-Core';
			package: 'OSWindow-X11' with: 'OSWindow-X11';
			package: 'OSWindow-TUIO' with: 'OSWindow-TUIO';
			package: 'OSWindow-SDL2-Examples' with: 'OSWindow-SDL2-Examples-MerwanOuddane.7';
			package: 'OSWindow-Core-Extras' with: 'OSWindow-Core-Extras'. ].
