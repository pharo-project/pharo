versions
version05: spec
	<version: '0.5' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'Cursor navigation refactored and improved'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-DenisKudrayshov.9';
			package: 'TxText-Layout' with: 'TxText-Layout-DenisKudrayshov.18';
			package: 'TxText-UI' with: 'TxText-UI-DenisKudrayshov.11';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-DenisKudrayshov.5';
			package: 'TxTextTests-Layout' with: 'TxTextTests-Layout-DenisKudrayshov.16';
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].
