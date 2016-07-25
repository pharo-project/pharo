versions
version04: spec
	<version: '0.4' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'merge with Tristan work and layout cursor positioning'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-DenisKudryashov.6';
			package: 'TxText-Layout' with: 'TxText-Layout-DenisKudryashov.14';
			package: 'TxText-UI' with: 'TxText-UI-DenisKudryashov.8';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-DenisKudryashov.4';
			package: 'TxTextTests-Layout' with: 'TxTextTests-Layout-DenisKudryashov.13';
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].
