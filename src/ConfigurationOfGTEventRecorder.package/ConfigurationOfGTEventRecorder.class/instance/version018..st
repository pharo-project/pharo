versions
version018: spec
	<version: '0.18' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit Version 3.23'.
		spec author: 'AndreiChis'.
		spec timestamp: '6/27/2016 12:09'.
		spec project: 'Ston' with: '0.19'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-YuriyTymchuk.69';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.37'. ].
