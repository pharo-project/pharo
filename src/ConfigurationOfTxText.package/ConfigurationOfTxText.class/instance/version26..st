versions
version26: spec
	<version: '2.6' imports: #('0.9-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'New baseline without mocketry'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-IgorStasenko.46';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-IgorStasenko.24';
			package: 'TxText-Athens' with: 'TxText-Athens-IgorStasenko.34';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-IgorStasenko.9';
		 	package: 'TxText-Styler' with: 'TxText-Styler-IgorStasenko.2';
			
			project: 'Athens' with: '2.5'
	].
