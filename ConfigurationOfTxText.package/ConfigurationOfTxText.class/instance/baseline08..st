baselines
baseline08: spec
	<version: '0.8-baseline'>

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
			
				
			package: 'TxText-Model';
			package: 'TxTextTests-Model' with: [ 
				spec requires: #('TxText-Model' 'Mocketry default' )];
			
			
			package: 'TxText-Athens' with: [ spec requires: #('TxText-Model') ];
			package: 'TxText-AthensTests' with: [ spec requires: #('TxText-Athens') ];
			package: 'TxText-Styler' with: [ spec requires: #('TxText-Athens') ].

			
		spec
			group: 'default' with: #('Core' 'Tests');
			group: 'Core' with: #('TxText-Model' 'TxText-Athens' 'TxText-Styler' );
			group: 'Tests' with: #('Core' 'TxTextTests-Model' 'Mocketry Tests' 'TxText-AthensTests'). 
			
	].
