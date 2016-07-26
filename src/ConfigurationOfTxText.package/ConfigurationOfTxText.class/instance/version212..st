versions
version212: spec
	<version: '2.12' imports: #('1.0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Load new TxText version
issue 16078:
collecting recent changes:
16047 TxTextStyler needs to implement visitGlobalNode:
15559 TxTextEditorMorph default behavior is unsafe'.
		spec author: 'NicolaiHess'.
		spec timestamp: '2015-08-06T23:54:57.550758+02:00'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-SeanDeNigris.61';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-SeanDeNigris.29';
			package: 'TxText-Athens' with: 'TxText-Athens-NicolaiHess.52';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-SeanDeNigris.10';
			package: 'TxText-Styler' with: 'TxText-Styler-NicolaiHess.11';
			package: 'Tool-TxWorkspace' with: 'Tool-TxWorkspace-NicolaiHess.7'. ].