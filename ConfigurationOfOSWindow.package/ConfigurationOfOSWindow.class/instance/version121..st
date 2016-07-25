versions
version121: spec
	<version: '1.2.1' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: '- fix event handling & conversion for Mac platform(s)'.
		spec author: 'IgorStasenko'.
		spec timestamp: '8/18/2015 16:19'.
		spec 
			project: 'Athens' with: '3.4';
			project: 'NBOpenGL' .
		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-IgorStasenko.56';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-IgorStasenko.54';
			package: 'OSWindow-VM' with: 'OSWindow-VM-IgorStasenko.3' ].
