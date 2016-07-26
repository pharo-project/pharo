versions
version211: spec
	<version: '2.11' imports: #('1.0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'merge lates changes'.
		spec author: 'NicolaiHess'.
		spec timestamp: '2015-07-26T19:19:00.943441+02:00'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-SeanDeNigris.61';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-SeanDeNigris.29';
			package: 'TxText-Athens' with: 'TxText-Athens-NicolaiHess.51';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-SeanDeNigris.10';
			package: 'TxText-Styler' with: 'TxText-Styler-ThomasHeniart.9';
			package: 'Tool-TxWorkspace' with: 'Tool-TxWorkspace-NicolaiHess.7'. ].