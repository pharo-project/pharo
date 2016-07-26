versions
version22: spec
	<version: '2.2' imports: #('0.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'TxTextMorph with more recent workings'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-IgorStasenko.38';
		 
			package: 'TxText-OldUI' with: 'TxText-OldUI-IgorStasenko.1';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-IgorStasenko.20';


		 
 			package: 'StructuredEditor' with: 'StructuredEditor-CamilleTeruel.11';
			package: 'TxText-Athens' with: 'TxText-Athens-IgorStasenko.18';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-IgorStasenko.3';
			
			project: 'Athens' with: '2.5';
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].
