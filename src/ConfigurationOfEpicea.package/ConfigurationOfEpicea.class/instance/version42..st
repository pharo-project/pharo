versions
version42: spec
	<version: '4.2' imports: #('4.1-baseline') >

	spec for: #'common' do: [
		spec description: '
	* Put an upper bound to number of entries displayed in the drag&drop morph.
	* Clean-up. Several passes of CodeCritics. 
	* Use standard Pharo icons in browser.
	* Fixed other minor issues.
'.
		spec author: 'MartinDias'.
		spec timestamp: '31/07/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.163';
			package: 'Epicea' with: 'Epicea-MartinDias.627'. ].