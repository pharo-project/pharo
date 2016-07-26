versions
version129: spec
	<version: '1.2.9' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.9'.
		spec author: 'AndreiChis'.
		spec timestamp: '8/12/2015 15:31'.
		spec project: 'GlamourCore' with: '3.1.9'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AndreiChis.318';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-AndreiChis.92';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TorstenBergmann.154';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AndreiChis.9'. ].
