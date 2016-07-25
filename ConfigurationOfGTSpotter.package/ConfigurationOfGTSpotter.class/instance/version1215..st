versions
version1215: spec
	<version: '1.2.15' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'bugfixes'.
		spec author: 'AndreiChis'.
		spec timestamp: '10/15/2015 16:22'.
		spec project: 'GlamourCore' with: '3.2.5'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AndreiChis.325';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-MarcusDenker.97';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.166';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AndreiChis.14'. ].
