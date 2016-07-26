baselines
baseline02: spec
	<version: '0.2-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/Pharo/OSWindow/main/'.
		spec 
			package: 'OSWindow-Core';
			package: 'OSWindow-SDL2' with: [
				spec requires: #('OSWindow-Core' ). ];
			package: 'OSWindow-VM' with: [
				spec requires: #('OSWindow-Core' ). ];
			package: 'NBXLib-Core';
			package: 'OSWindow-X11' with: [
				spec requires: #('OSWindow-Core' 'NBXLib-Core' ). ].
		
		spec 
			group: 'SDL2' with: #('OSWindow-SDL2' );
			group: 'Legacy' with: #('OSWindow-VM' );
			group: 'X11' with: #('OSWindow-X11' );
			group: 'default' with: #('Legacy' 'SDL2' ). ].
