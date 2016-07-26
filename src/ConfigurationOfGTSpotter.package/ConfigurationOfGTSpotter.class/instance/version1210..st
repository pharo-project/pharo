versions
version1210: spec
	<version: '1.2.10' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.10'.
		spec author: 'AndreiChis'.
		spec timestamp: '9/1/2015 15:31'.
		spec project: 'GlamourCore' with: '3.1.10'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AndreiChis.318';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-YuriyTymchuk.94';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-MarcusDenker.156';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AndreiChis.9'. ].
