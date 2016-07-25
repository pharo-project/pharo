baselines
baseline01: spec
	<version: '0.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/Pharo/OSWindow/main/'.
		spec 
			package: 'OSWindow-Core';
			package: 'OSWindow-SDL2' with: [
				spec requires: #('OSWindow-Core' ). ];
			package: 'OSWindow-VM' with: [
				spec requires: #('OSWindow-Core' ). ];
			package: 'OSWindow-X11' with: [
				spec requires: #('OSWindow-Core' 'NBXLib-Core' ). ];
			package: 'NBXLib-Core'. ].
