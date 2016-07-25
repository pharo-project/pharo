versions
version23: spec
	<version: '2.3' imports: #('0.7-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'Removed unused stuff'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-IgorStasenko.41';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-IgorStasenko.20';
		 
 			package: 'StructuredEditor' with: 'StructuredEditor-CamilleTeruel.11';
			package: 'TxText-Athens' with: 'TxText-Athens-StephaneDucasse.31';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-SeanDeNigris.7';
			
			project: 'Athens' with: '2.5';
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].
