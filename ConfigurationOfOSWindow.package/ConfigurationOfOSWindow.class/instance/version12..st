versions
version12: spec
	<version: '1.2' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'Make event handling works for morphic'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '8/11/2015 14:10'.
		spec 
			project: 'Athens' with: '3.4';
			project: 'NBOpenGL' .
		spec 
			package: 'OSWindow-Core' with: 'OSWindow-Core-IgorStasenko.55';
			package: 'OSWindow-SDL2' with: 'OSWindow-SDL2-IgorStasenko.53';
			package: 'OSWindow-VM' with: 'OSWindow-VM-IgorStasenko.3' ]