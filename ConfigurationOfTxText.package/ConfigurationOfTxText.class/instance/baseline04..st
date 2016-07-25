baselines
baseline04: spec
	<version: '0.4-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://www.smalltalkhub.com/mc/sig/TxText/main'.
		spec 
			project: 'Mocketry default' with: [
		     		spec 
					className: 'ConfigurationOfMocketry';
					loads: 'default';
					repository: 'http://www.smalltalkhub.com/mc/dionisiy/Mocketry/main'];  

			project: 'Mocketry Tests' with: [
		     		spec 
					className: 'ConfigurationOfMocketry';
					loads: 'Tests';					
					repository: 'http://www.smalltalkhub.com/mc/dionisiy/Mocketry/main'];  
				
			package: 'StructuredEditor' with: [
				spec repository: 'http://www.smalltalkhub.com/mc/CamilleTeruel/StructuredEditor/main'
			]; 
				
				
			package: 'TxText-Model';
			package: 'TxText-Layout' with: [
				spec requires: 'TxText-Model']; 
			package: 'TxText-Editor' with: [
				spec requires: 'TxText-Layout']; 
			package: 'TxText-UI' with: [
				spec requires: 'TxText-Editor'];
			package: 'TxTextTests-Model' with: [ 
				spec requires: #('TxText-Model' 'Mocketry default' )];
			package: 'TxTextTests-Layout' with: [ 
				spec requires: #('TxText-Layout' 'Mocketry default')];
			package: 'TxTextTests-Editor' with: [ 
				spec requires: #('TxText-Editor' 'Mocketry default')];
			
			package: 'TxText-Athens' with: [ spec requires: #('StructuredEditor') ];
			package: 'TxText-AthensTests' with: [ spec requires: #('TxText-Athens') ]
			
			.
			
		spec
			group: 'default' with: #('Core' 'Tests');
			group: 'Core' with: #('TxText-Model' 'TxText-Layout' 'TxText-UI' 'TxText-Athens' );
			group: 'Tests' with: #('Core' 'TxTextTests-Model' 'TxTextTests-Layout' 'TxTextTests-Editor' 'Mocketry Tests' 'TxText-AthensTests'). 
			
	].
