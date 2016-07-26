versions
version03: spec
	<version: '0.3' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'merge with Tristan work'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-TristanBourgois.3';
			package: 'TxText-Layout' with: 'TxText-Layout-DenisKudryashov.3';
			package: 'TxText-UI' with: 'TxText-UI-DenisKudryashov.3';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-TristanBourgois.3';
			package: 'TxTextTests-Layout' with: 'TxTextTests-Layout-DenisKudryashov.3';
			project: 'Mocketry default' with: '2.1';
			project: 'Mocketry Tests' with: '2.1'
	].
