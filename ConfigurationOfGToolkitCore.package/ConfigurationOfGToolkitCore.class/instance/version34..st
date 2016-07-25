versions
version34: spec
	<version: '3.4' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.4'.
		spec author: 'JurajKubelka'.
		spec timestamp: '1/7/2016 19:36'.
		spec 
			project: 'GTPlaygroundCore' with: '2.4';
			project: 'GTInspectorCore' with: '2.3';
			project: 'GTSpotter' with: '1.6';
			project: 'GTDebugger' with: '1.1'. ].
