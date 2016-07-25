versions
version27: spec
	<version: '2.7' imports: #('0.9-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'Update to fix character stream issues.'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-IgorStasenko.48';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-IgorStasenko.26';
			package: 'TxText-Athens' with: 'TxText-Athens-IgorStasenko.35';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-IgorStasenko.9';
		 	package: 'TxText-Styler' with: 'TxText-Styler-IgorStasenko.3';
			
			
			project: 'Athens' with: '2.5'
	].
