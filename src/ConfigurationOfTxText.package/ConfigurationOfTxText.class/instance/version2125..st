versions
version2125: spec
	<version: '2.12.5' imports: #('1.0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Removes a Halt in TxModel >> #postCopy, because it only has a halt. Issue #18073'.
		spec author: 'PabloTesone'.
		spec timestamp: '24 June 2016, 12:20'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-PabloTesone.66';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-SeanDeNigris.29';
			package: 'TxText-Athens' with: 'TxText-Athens-GuillermoPolito.53';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-SeanDeNigris.10';
			package: 'TxText-Styler' with: 'TxText-Styler-MarcusDenker.17';
			package: 'Tool-TxWorkspace' with: 'Tool-TxWorkspace-NicolaiHess.7'. ].
