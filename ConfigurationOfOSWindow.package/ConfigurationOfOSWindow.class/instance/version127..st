versions
version127: spec
	<version: '1.2.7' imports: #('0.7-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.7'.
		spec author: 'RonieSalgado'.
		spec timestamp: '4/22/2016 09:41'.
		spec 
			project: 'Athens' with: '3.9.3';
			project: 'UFFIOpenGL' with: '1.0'.
		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-RonieSalgado.103';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-RonieSalgado.95';
			package: 'OSWindow-VM' with: 'OSWindow-VM-MerwanOuddane.4';
			package: 'NBXLib-Core' with: 'NBXLib-Core-jeanbaptistearnaud.10';
			package: 'OSWindow-X11' with: 'OSWindow-X11';
			package: 'OSWindow-TUIO' with: 'OSWindow-TUIO';
			package: 'OSWindow-SDL2-Examples' with: 'OSWindow-SDL2-Examples-RonieSalgado.11';
			package: 'OSWindow-GLExample' with: 'OSWindow-GLExample-RonieSalgado.5'. ].
