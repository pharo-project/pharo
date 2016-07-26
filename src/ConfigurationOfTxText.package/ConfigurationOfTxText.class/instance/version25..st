versions
version25: spec
	<version: '2.5' imports: #('0.8-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'New text styler'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-IgorStasenko.46';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-IgorStasenko.22';
			package: 'TxText-Athens' with: 'TxText-Athens-IgorStasenko.34';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-IgorStasenko.8';
		 	package: 'TxText-Styler' with: 'TxText-Styler-IgorStasenko.2';
			
			project: 'Athens' with: '2.5';
			project: 'Mocketry default' with: '2.3';
			project: 'Mocketry Tests' with: '2.3'
	].
