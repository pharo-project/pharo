versions
version123: spec
	<version: '1.2.3' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Replace Announcer>>#on:send:to:''s senders with Announcer>>#when:send:to: in the following methods

GTSpotterCategoryTitleBrick>>#stepModel: 
GTSpotterMorph>>#initializeListeners'.
		spec author: 'BernardoContreras'.
		spec timestamp: '5/2/2015 18:48'.
		spec project: 'GlamourCore' with: '3.1.2'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-BernardoContreras.308';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.89';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.142';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-TudorGirba.8'. ].
