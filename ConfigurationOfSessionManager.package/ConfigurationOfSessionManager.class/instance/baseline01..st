baselines
baseline01: spec
	<version: '0.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/Pharo/SessionManager/main/'.
		spec 
			package: #'System-SessionManager';
			package: #'System-SessionManager-Tests' with: [
				spec requires: #(#'System-SessionManager' ). ]. ].
