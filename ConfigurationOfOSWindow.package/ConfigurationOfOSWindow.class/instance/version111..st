versions
version111: spec
	<version: '1.1.1' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: 'version 1.1.1'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2014-08-19 13:51'.
		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-EstebanLorenzano.17';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-EstebanLorenzano.19';
			package: 'OSWindow-VM' with: 'OSWindow-VM-IgorStasenko.2';
			package: 'NBXLib-Core' with: 'NBXLib-Core-RonieSalgado.13';
			package: 'OSWindow-X11' with: 'OSWindow-X11-IgorStasenko.5'. ].
