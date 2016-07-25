versions
v0_1_4: spec
	<version: '0.1.4'>
	
	spec
		for: #'common'
		do: [ 
			spec
				baseline: 'OSSubprocess'
					with: [ spec repository: 'github://marianopeck/OSSubprocess:v0.1.4/repository' ];
				import: 'OSSubprocess' ].