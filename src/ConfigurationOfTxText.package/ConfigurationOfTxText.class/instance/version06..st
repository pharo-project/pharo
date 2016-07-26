versions
version06: spec
	<version: '0.6' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'edit and selection behaviour done. TxTextMorph live'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-DenisKudrayshov.19';
			package: 'TxText-Layout' with: 'TxText-Layout-DenisKudrayshov.27';
			package: 'TxText-UI' with: 'TxText-UI-DenisKudrayshov.21';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-DenisKudrayshov.9';
			package: 'TxTextTests-Layout' with: 'TxTextTests-Layout-DenisKudrayshov.19';
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].
