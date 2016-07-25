versions
version012: spec
	<version: '0.12' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Fix for issue 17836'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/17/2016 15:05'.
		spec project: 'Ston' with: '0.17'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-AndreiChis.62';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.36'. ].
