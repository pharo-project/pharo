baselines
baseline06: spec
	<version: '0.6-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://www.smalltalkhub.com/mc/sig/TxText/main'.
		spec 
			project: 'Mocketry default' with: [
		     		spec 
					className: 'ConfigurationOfMocketry';
					loads: 'default';
					repository: 'http://www.smalltalkhub.com/mc/dionisiy/Mocketry/main'];  
	
			project: 'Athens' with: [
			    spec 
				className: 'ConfigurationOfAthens';
				loads: 'default';
				repository: 'http://www.smalltalkhub.com/mc/Pharo/Athens/main'];  

			project: 'Mocketry Tests' with: [
		     		spec 
					className: 'ConfigurationOfMocketry';
					loads: 'Tests';					
					repository: 'http://www.smalltalkhub.com/mc/dionisiy/Mocketry/main'];  
				
			package: 'StructuredEditor' with: [
				spec repository: 'http://www.smalltalkhub.com/mc/CamilleTeruel/StructuredEditor/main'
			]; 
				
				
			package: 'TxText-Model';
			package: 'TxText-OldLayout' with: [
				spec requires: 'TxText-Model']; 
			package: 'TxText-OldEditor' with: [
				spec requires: 'TxText-OldLayout']; 
			package: 'TxText-OldUI' with: [
				spec requires: 'TxText-OldEditor'];
			package: 'TxTextTests-Model' with: [ 
				spec requires: #('TxText-Model' 'Mocketry default' )];
			package: 'TxTextTests-OldLayout' with: [ 
				spec requires: #('TxText-OldLayout' 'Mocketry default')];
			package: 'TxTextTests-OldEditor' with: [ 
				spec requires: #('TxText-OldEditor' 'Mocketry default')];
			
			package: 'TxText-Athens' with: [ spec requires: #('StructuredEditor') ];
			package: 'TxText-AthensTests' with: [ spec requires: #('TxText-Athens') ]
			
			.
			
		spec
			group: 'default' with: #('Core' 'Tests');
			group: 'Core' with: #('TxText-Model' 'TxText-OldLayout' 'TxText-OldUI' 'TxText-Athens' );
			group: 'Tests' with: #('Core' 'TxTextTests-Model' 'TxTextTests-OldLayout' 'TxTextTests-OldEditor' 'Mocketry Tests' 'TxText-AthensTests'). 
			
	].
