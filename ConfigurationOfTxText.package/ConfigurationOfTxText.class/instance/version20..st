versions
version20: spec
	<version: '2.0' imports: #('0.4-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'TxTextMorph with more recent workings'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-IgorStasenko.37';
		 
			package: 'TxText-UI' with: 'TxText-UI-IgorStasenko.39';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-IgorStasenko.20';


		 
 			package: 'StructuredEditor' with: 'StructuredEditor-CamilleTeruel.11';
			package: 'TxText-Athens' with: 'TxText-Athens-IgorStasenko.17';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-IgorStasenko.3';
			
			
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].
