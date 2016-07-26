versions
version10: spec
	<version: '1.0' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'Edit validation logic with text specs'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-DenisKudrayshov.27';
			package: 'TxText-Layout' with: 'TxText-Layout-DenisKudrayshov.33';
			package: 'TxText-Editor' with: 'TxText-Editor-DenisKudrayshov.17';
			package: 'TxText-UI' with: 'TxText-UI-DenisKudrayshov.33';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-DenisKudrayshov.14';
			package: 'TxTextTests-Layout' with: 'TxTextTests-Layout-DenisKudrayshov.22';
			package: 'TxTextTests-Editor' with: 'TxTextTests-Editor-DenisKudrayshov.15';
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].
