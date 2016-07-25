versions
version014: spec
	<version: '0.14' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.17'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/29/2016 23:44'.
		spec project: 'Ston' with: '0.17'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-AndreiChis.64';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.37'. ].
