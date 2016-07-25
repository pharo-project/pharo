versions
version203: spec
	<version: '2.0.3' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.3'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/07/2014 13:58'.
		
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-EstebanLorenzano.271';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.86';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.24'. 
		spec 
			project: 'GlamourCore' with: '3.0.3'. ].
