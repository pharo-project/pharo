versions
version09: spec
	<version: '0.9' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'Edit decorators renamings and new TxSecretEditDecorator'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-DenisKudrayshov.26';
			package: 'TxText-Layout' with: 'TxText-Layout-DenisKudrayshov.33';
			package: 'TxText-Editor' with: 'TxText-Editor-DenisKudrayshov.14';
			package: 'TxText-UI' with: 'TxText-UI-DenisKudrayshov.30';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-DenisKudrayshov.13';
			package: 'TxTextTests-Layout' with: 'TxTextTests-Layout-DenisKudrayshov.22';
			package: 'TxTextTests-Editor' with: 'TxTextTests-Editor-DenisKudrayshov.12';
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].
