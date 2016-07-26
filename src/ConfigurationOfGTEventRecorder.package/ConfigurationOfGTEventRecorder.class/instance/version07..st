versions
version07: spec
	<version: '0.7' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Fixes in the debugger, inspector and spotter.'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/10/2016 23:53'.
		spec project: 'Ston' with: '0.14'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-JurajKubelka.53';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.32'. ].
