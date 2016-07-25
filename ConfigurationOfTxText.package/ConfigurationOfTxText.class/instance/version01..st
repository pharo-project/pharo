versions
version01: spec
	<version: '0.1' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'TxText is new text model system. It is possible replacement of old text editors stuff'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-DenisKudryashov.1';
			package: 'TxText-Layout' with: 'TxText-Layout-DenisKudryashov.1';
			package: 'TxText-UI' with: 'TxText-UI-DenisKudryashov.1';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-DenisKudryashov.1';
			package: 'TxTextTests-Layout' with: 'TxTextTests-Layout-DenisKudryashov.1'
	].
