versions
version127: spec
	<version: '1.2.7' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.7'.
		spec author: 'AndreiChis'.
		spec timestamp: '7/1/2015 14:25'.
		spec project: 'GlamourCore' with: '3.1.7'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-NicolaiHess.310';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-AndreiChis.92';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-NicolaiHess.148';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-TudorGirba.8'. ].
