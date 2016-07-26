versions
version11: spec
	<version: '1.1' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'TxTextMorph with halo menu items and correct transparent drawing'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-DenisKudrayshov.28';
			package: 'TxText-Layout' with: 'TxText-Layout-DenisKudrayshov.34';
			package: 'TxText-Editor' with: 'TxText-Editor-DenisKudrayshov.17';
			package: 'TxText-UI' with: 'TxText-UI-DenisKudrayshov.37';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-DenisKudrayshov.15';
			package: 'TxTextTests-Layout' with: 'TxTextTests-Layout-DenisKudrayshov.22';
			package: 'TxTextTests-Editor' with: 'TxTextTests-Editor-DenisKudrayshov.15';
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].
