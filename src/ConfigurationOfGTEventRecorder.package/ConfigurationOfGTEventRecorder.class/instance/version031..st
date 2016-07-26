versions
version031: spec
	<version: '0.3.1' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Changes in the use of #garbageCollect'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/25/2015 18:00'.
		spec project: 'Ston' with: '0.14'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-JurajKubelka.50';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.30'. ].
