versions
version2121: spec
	<version: '2.12.1' imports: #('1.0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '16122 TextLink positions are wrong
16121 TxTextStyler can not process RBSelfNode'.
		spec author: 'MarcusDenker'.
		spec timestamp: '10/8/2015 15:03'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-SeanDeNigris.61';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-SeanDeNigris.29';
			package: 'TxText-Athens' with: 'TxText-Athens-NicolaiHess.52';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-SeanDeNigris.10';
			package: 'TxText-Styler' with: 'TxText-Styler-MarcusDenker.17';
			package: 'Tool-TxWorkspace' with: 'Tool-TxWorkspace-NicolaiHess.7'. ].
