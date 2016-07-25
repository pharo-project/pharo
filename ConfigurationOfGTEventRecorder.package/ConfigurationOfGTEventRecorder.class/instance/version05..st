versions
version05: spec
	<version: '0.5' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 0.5. It fixes issues 17222 and 17341.'.
		spec author: 'JurajKubelka'.
		spec timestamp: '1/7/2016 19:20'.
		spec project: 'Ston' with: '0.14'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-JurajKubelka.53';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.32'. ].
