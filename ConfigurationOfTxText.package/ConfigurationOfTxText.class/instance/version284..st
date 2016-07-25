versions
version284: spec
	<version: '2.8.4' imports: #('1.0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'Issue 15414: TxText Cleanup for Pharo 5.0 #1'.
		spec timestamp: '2015-04-27T17:23:16.32781+02:00'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-SeanDeNigris.53';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-IgorStasenko.26';
			package: 'TxText-Athens' with: 'TxText-Athens-SeanDeNigris.46';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-IgorStasenko.9';
		 	package: 'TxText-Styler' with: 'TxText-Styler-SeanDeNigris.4';
			package: 'Tool-TxWorkspace' with: 'Tool-TxWorkspace-TorstenBergmann.5' ]