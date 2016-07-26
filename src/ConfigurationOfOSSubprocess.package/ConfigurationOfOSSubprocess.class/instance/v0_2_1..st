versions
v0_2_1: spec
	<version: '0.2.1'>
	
	spec
		for: #'common'
		do: [ 
			spec
				baseline: 'OSSubprocess'
					with: [ spec repository: 'github://marianopeck/OSSubprocess:v0.2.1/repository' ];
				import: 'OSSubprocess' ].