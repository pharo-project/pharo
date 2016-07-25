baselines
baseline01: spec
	<version: '0.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://www.smalltalkhub.com/mc/sig/TxText/main'.
		spec 
			package: 'TxText-Model';
			package: 'TxText-Layout' with: [
				spec requires: 'TxText-Model']; 
			package: 'TxText-UI' with: [
				spec requires: 'TxText-Layout'];
			package: 'TxTextTests-Model' with: [ 
				spec requires: 'TxText-Model' ];
			package: 'TxTextTests-Layout' with: [ 
				spec requires: 'TxText-Layout' ]
	].
