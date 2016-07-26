versions
version010: spec
	<version: '0.10' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.14'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/28/2016 12:11'.
		spec project: 'Ston' with: '0.17'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-AndreiChis.60';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.36'. ].
