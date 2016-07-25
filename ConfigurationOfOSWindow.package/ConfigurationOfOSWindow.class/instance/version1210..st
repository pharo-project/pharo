versions
version1210: spec
	<version: '1.2.10' imports: #('0.8-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.9'.
		spec author: 'RonieSalgado'.
		spec timestamp: '4/22/2016 14:25'.
		spec 
			project: 'Athens' with: '3.9.3';
			project: 'UFFIOpenGL' with: '1.0.1'.
		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-GuillermoPolito.105';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-RonieSalgado.95';
			package: 'OSWindow-VM' with: 'OSWindow-VM-MerwanOuddane.4';
			package: 'NBXLib-Core' with: 'NBXLib-Core';
			package: 'OSWindow-X11' with: 'OSWindow-X11';
			package: 'OSWindow-TUIO' with: 'OSWindow-TUIO';
			package: 'OSWindow-SDL2-Examples' with: 'OSWindow-SDL2-Examples-RonieSalgado.12';
			package: 'OSWindow-GLExample' with: 'OSWindow-GLExample';
			package: 'OSWindow-Tests' with: 'OSWindow-Tests-RonieSalgado.3'. ].
