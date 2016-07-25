versions
version13: spec
	<version: '1.3' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Updated GTools: 
  - bug fixes
  - deprecated PluggableTextMorph
  - improved event recorder'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/9/2015 14:32'.
		spec 
			project: 'GlamourCore' with: '3.3';
			project: 'GTEventRecorder' with: '0.1'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AndreiChis.333';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.102';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.166';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AndreiChis.14'. ].
