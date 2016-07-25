baselines
baseline02: spec
	<version: '0.2-baseline'>

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
			package: 'TxText-Model';
			package: 'TxText-Layout' with: [
				spec requires: 'TxText-Model']; 
			package: 'TxText-UI' with: [
				spec requires: 'TxText-Layout'];
			package: 'TxTextTests-Model' with: [ 
				spec requires: #('TxText-Model' 'Mocketry default' )];
			package: 'TxTextTests-Layout' with: [ 
				spec requires: #('TxText-Layout' 'Mocketry default')].
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('TxText-Model' 'TxText-Layout' 'TxText-UI');
			group: 'Tests' with: #('TxTextTests-Model' 'TxTextTests-Layout' 'Mocketry Tests'). 
			
	].
