versions
version02: spec
	<version: '0.2' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 0.2'.
		spec author: 'JurajKubelka'.
		spec timestamp: '11/15/2015 19:01'.
		spec project: 'Ston' with: '0.14'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-JurajKubelka.36';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.22'. ].
