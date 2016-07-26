versions
version03: spec
	<version: '0.3' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Update GTools. 
 - bug fixes
 - improvements in the event recorder'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/19/2015 20:58'.
		spec project: 'Ston' with: '0.14'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-AndreiChis.49';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-AndreiChis.29'. ].
