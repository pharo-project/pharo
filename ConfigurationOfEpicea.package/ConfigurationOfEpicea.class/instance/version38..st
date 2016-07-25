versions
version38: spec
	<version: '3.8' imports: #('3.3-baseline') >

	spec for: #'common' do: [
		spec description: 'Added a group for World Menu'.
		spec author: 'MartinDias'.
		spec timestamp: '03/05/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.154';
			package: 'Epicea' with: 'Epicea-MartinDias.585'. ].