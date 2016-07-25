versions
version281: spec
	<version: '2.8.1' imports: #('1.0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec timestamp: '2014-12-08 16:52'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-IgorStasenko.48';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-IgorStasenko.26';
			package: 'TxText-Athens' with: 'TxText-Athens-IgorStasenko.35';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-IgorStasenko.9';
		 	package: 'TxText-Styler' with: 'TxText-Styler-IgorStasenko.3';
			package: 'Tool-TxWorkspace' with: 'Tool-TxWorkspace-IgorStasenko.3' ]