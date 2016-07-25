versions
version02: spec
	<version: '0.2' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'TxText is new text model system. It is possible replacement of old text editors stuff'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-DenisKudryashov.2';
			package: 'TxText-Layout' with: 'TxText-Layout-DenisKudryashov.2';
			package: 'TxText-UI' with: 'TxText-UI-DenisKudryashov.2';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-DenisKudryashov.2';
			package: 'TxTextTests-Layout' with: 'TxTextTests-Layout-DenisKudryashov.2';
			project: 'Mocketry default' with: '2.1';
			project: 'Mocketry Tests' with: '2.1'
	].
