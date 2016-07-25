versions
version50: spec
	<version: '5.0' imports: #('5.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Remove tolerance for corrupt files. Not needed and error-prone.
	* Fix for Ombu in Linux (flush after writing a new entry).
	* Stop support for Pharo 30. Still not working in Pharo 50, but I created a compatibility package for Pharo 40 which will help.
	* Some minor clean-ups.
	* Clean up and small fix for importing .changes
	* Added extension methods from EpiceaSorter
	* Improved LogBuilder (for tests)
	* Added concept of secondary color to log items
	* #imageName has been deprecated in Pharo 50 => changing to a selector that works in both 40 and 50.
	* Fixed #methodRemoved:'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-07-07 17:34'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.180';
			package: 'Epicea' with: 'Epicea-MartinDias.694';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.3'. ].

