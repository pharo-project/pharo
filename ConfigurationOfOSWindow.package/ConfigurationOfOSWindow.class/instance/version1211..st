versions
version1211: spec
	<version: '1.2.11' imports: #('0.8-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.11'.
		spec author: 'RonieSalgado'.
		spec timestamp: '4/26/2016 14:36'.
		spec 
			project: 'Athens' with: '3.9.3';
			project: 'UFFIOpenGL' with: '1.2'.
		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-RonieSalgado.107';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-RonieSalgado.96';
			package: 'OSWindow-VM' with: 'OSWindow-VM-MerwanOuddane.4';
			package: 'NBXLib-Core' with: 'NBXLib-Core-jeanbaptistearnaud.10';
			package: 'OSWindow-X11' with: 'OSWindow-X11-IgorStasenko.5';
			package: 'OSWindow-TUIO' with: 'OSWindow-TUIO-MerwanOuddane.15';
			package: 'OSWindow-SDL2-Examples' with: 'OSWindow-SDL2-Examples-RonieSalgado.13';
			package: 'OSWindow-GLExample' with: 'OSWindow-GLExample-RonieSalgado.5';
			package: 'OSWindow-Tests' with: 'OSWindow-Tests-RonieSalgado.3'. ].
