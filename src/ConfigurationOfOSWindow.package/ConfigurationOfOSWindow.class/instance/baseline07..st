baselines
baseline07: spec
	<version: '0.7-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/Pharo/OSWindow/main/'.
		spec 
			project: 'Athens' with: [
				spec
					className: #ConfigurationOfAthens;
					versionString: #'stable';
					repository: 'http://smalltalkhub.com/mc/Pharo/Athens/main/' ];
			project: 'UFFIOpenGL' with: [
				spec
					className: #ConfigurationOfUFFIOpenGL;
					versionString: #'stable';
					repository: 'http://smalltalkhub.com/mc/ronsaldo/UFFIOpenGL/main/' ].
		spec 
			package: 'OSWindow-Core';
			package: 'OSWindow-SDL2' with: [
				spec requires: #('OSWindow-Core' 'Athens' ). ];
			package: 'OSWindow-VM' with: [
				spec requires: #('OSWindow-Core' ). ];
			package: 'NBXLib-Core';
			package: 'OSWindow-X11' with: [
				spec requires: #('OSWindow-Core' 'NBXLib-Core' ). ];
			package: 'OSWindow-TUIO';
			package: 'OSWindow-SDL2-Examples';
			package: 'OSWindow-GLExample' with: [
				spec requires: #('UFFIOpenGL' ). ].
		spec 
			group: 'Examples' with: #('SDL2-Examples' );
			group: 'SDL2-Examples' with: #('OSWindow-SDL2-Examples' 'SDL2' );
			group: 'OpenGLExamples' with: #('OSWindow-GLExample' );
			group: 'X11' with: #('OSWindow-X11' );
			group: 'TUIO' with: #('SDL2' 'OSWindow-TUIO' );
			group: 'OpenGL' with: #();
			group: 'WithOpenGL' with: #('Legacy' 'SDL2' 'OpenGL' );
			group: 'WithOpenGLAndExamples' with: #('Legacy' 'SDL2' 'OpenGLExamples' );
			group: 'default' with: #('Legacy' 'SDL2' 'Examples' );
			group: 'Legacy' with: #('OSWindow-VM' );
			group: 'SDL2' with: #('OSWindow-SDL2' 'OSWindow-Core' ). ].
