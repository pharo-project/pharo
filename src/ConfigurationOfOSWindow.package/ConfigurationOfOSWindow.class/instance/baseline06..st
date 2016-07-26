baselines
baseline06: spec
	<version: '0.6-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/Pharo/OSWindow/main/'.
		spec 
			project: 'Athens' with: [
				spec
					className: #ConfigurationOfAthens;
					versionString: #'stable';
					repository: 'http://smalltalkhub.com/mc/Pharo/Athens/main/' ];
			project: 'NBOpenGL' with: [
				spec
					className: #ConfigurationOfNBOpenGL;
					versionString: #'stable';
					repository: 'http://smalltalkhub.com/mc/PharoExtras/NBOpenGL/main/' ].
		spec 
			package: 'OSWindow-Core';
			package: 'OSWindow-SDL2' with: [
				spec requires: #('OSWindow-Core' 'Athens' ). ];
			package: 'OSWindow-VM' with: [
				spec requires: #('OSWindow-Core' ). ];
			package: 'NBOpenGL-OSWindow' with: [
				spec requires: #('NBOpenGL' 'OSWindow-Core' ). ];
			package: 'NBXLib-Core';
			package: 'OSWindow-X11' with: [
				spec requires: #('OSWindow-Core' 'NBXLib-Core' ). ];
			package: 'OSWindow-TUIO';
			package: 'OSWindow-SDL2-Examples'.
		spec 
			group: 'SDL2' with: #('OSWindow-SDL2' 'OSWindow-Core' );
			group: 'Legacy' with: #('OSWindow-VM' );
			group: 'X11' with: #('OSWindow-X11' );
			group: 'OpenGL' with: #('NBOpenGL-OSWindow' );
			group: 'OpenGLExamples' with: #('NBOpenGL-OSWindow' );
			group: 'WithOpenGL' with: #('Legacy' 'SDL2' 'OpenGL' );
			group: 'WithOpenGLAndExamples' with: #('Legacy' 'SDL2' 'OpenGL' 'OpenGLExamples' );
			group: 'default' with: #('Legacy' 'SDL2' 'Examples' );
			group: 'TUIO' with: #('SDL2' 'OSWindow-TUIO' );
			group: 'Examples' with: #('SDL2-Examples' );
			group: 'SDL2-Examples' with: #('OSWindow-SDL2-Examples' 'SDL2' ). ].
