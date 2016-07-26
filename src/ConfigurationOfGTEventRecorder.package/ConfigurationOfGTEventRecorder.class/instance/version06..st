versions
version06: spec
	<version: '0.6' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'This version fixes problems in GTDebugger and updates  Rubric (issues: 17155, 17279, 17152)'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/8/2016 19:27'.
		spec project: 'Ston' with: '0.14'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-JurajKubelka.53';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.32'. ].
