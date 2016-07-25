versions
version24: spec
	<version: '2.4' imports: #('0.7-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'Removed unused stuff'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-IgorStasenko.45';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-IgorStasenko.21';
		 
			package: 'TxText-Athens' with: 'TxText-Athens-IgorStasenko.33';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-IgorStasenko.8';
			
			project: 'Athens' with: '2.5';
			project: 'Mocketry default' with: '2.3';
			project: 'Mocketry Tests' with: '2.3'
	].
