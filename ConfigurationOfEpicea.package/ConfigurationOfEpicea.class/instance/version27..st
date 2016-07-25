versions
version27: spec
	<version: '2.7' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Fix: EpMethodRemoval was not holding the real protocol.
			- Feature: Enable multiple selection in items list.
			- Clean up: removed unused code
			- Fix: undo and redo for multiple selection
			'.
		spec author: 'MartinDias'.
		spec timestamp: '19/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.128';
			package: 'Epicea' with: 'Epicea-MartinDias.511'.
			
		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version21PostLoad ].