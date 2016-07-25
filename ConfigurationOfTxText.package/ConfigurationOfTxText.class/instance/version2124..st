versions
version2124: spec
	<version: '2.12.4' imports: #('1.0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'issue 18072'.
		spec author: 'PavelKrivanek'.
		spec timestamp: '6/13/2016 11:05'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-PavelKrivanek.65';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-SeanDeNigris.29';
			package: 'TxText-Athens' with: 'TxText-Athens-GuillermoPolito.53';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-SeanDeNigris.10';
			package: 'TxText-Styler' with: 'TxText-Styler-MarcusDenker.17';
			package: 'Tool-TxWorkspace' with: 'Tool-TxWorkspace-NicolaiHess.7'. ].
