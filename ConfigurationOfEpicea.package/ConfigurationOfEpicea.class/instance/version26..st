versions
version26: spec
	<version: '2.6' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Ombu: Fix Ston materialization of RPackageTags.
			- Epicea: Added a button to absent entry, enabling navigation between ombu files.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '14/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.128';
			package: 'Epicea' with: 'Epicea-MartinDias.505'.
			
		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version21PostLoad ].