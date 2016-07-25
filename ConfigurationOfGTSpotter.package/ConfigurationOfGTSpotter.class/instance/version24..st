versions
version24: spec
	<version: '2.4' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools Version 3.15'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/17/2016 10:50'.
		spec 
			project: 'GlamourCore' with: '4.6';
			project: 'GTEventRecorder' with: '0.11'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.382';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-TheIntegrator.109';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.198';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-TudorGirba.16'. ].
