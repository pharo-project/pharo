versions
version11: spec
	<version: '1.1' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.1'.
		spec author: 'IgorStasenko'.
		spec timestamp: '7/30/2014 18:58'.
		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-IgorStasenko.16';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-IgorStasenko.18';
			package: 'OSWindow-VM' with: 'OSWindow-VM-IgorStasenko.2';
			package: 'NBXLib-Core' with: 'NBXLib-Core-RonieSalgado.13';
			package: 'OSWindow-X11' with: 'OSWindow-X11-IgorStasenko.5'. ].
