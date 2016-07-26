versions
version128: spec
	<version: '1.2.8' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.8'.
		spec author: 'AndreiChis'.
		spec timestamp: '7/3/2015 13:54'.
		spec project: 'GlamourCore' with: '3.1.8'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AndreiChis.315';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-AndreiChis.92';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.152';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AndreiChis.9'. ].
