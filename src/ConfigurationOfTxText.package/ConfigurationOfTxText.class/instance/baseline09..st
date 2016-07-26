baselines
baseline09: spec
	<version: '0.9-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://www.smalltalkhub.com/mc/sig/TxText/main'.
		spec 
			project: 'Athens' with: [
			    spec 
				className: 'ConfigurationOfAthens';
				loads: 'default';
				repository: 'http://www.smalltalkhub.com/mc/Pharo/Athens/main'];  

			package: 'TxText-Model';
			package: 'TxTextTests-Model' with: [ 
				spec requires: #('TxText-Model')];
			
			
			package: 'TxText-Athens' with: [ spec requires: #('TxText-Model') ];
			package: 'TxText-AthensTests' with: [ spec requires: #('TxText-Athens') ];
			package: 'TxText-Styler' with: [ spec requires: #('TxText-Athens') ].

			
		spec
			group: 'default' with: #('Core' 'Tests');
			group: 'Core' with: #('TxText-Model' 'TxText-Athens' 'TxText-Styler' );
			group: 'Tests' with: #('Core' 'TxTextTests-Model' 'TxText-AthensTests'). 
			
	].
