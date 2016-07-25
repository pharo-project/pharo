versions
version07: spec
	<version: '0.7' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'Standart shortcuts for navigation and editing done.'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-DenisKudrayshov.22';
			package: 'TxText-Layout' with: 'TxText-Layout-DenisKudrayshov.31';
			package: 'TxText-UI' with: 'TxText-UI-DenisKudrayshov.25';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-DenisKudrayshov.12';
			package: 'TxTextTests-Layout' with: 'TxTextTests-Layout-DenisKudrayshov.21';
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].
