versions
version013: spec
	<version: '0.13' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.16'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/23/2016 14:21'.
		spec project: 'Ston' with: '0.17'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-AndreiChis.62';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.36'. ].
