versions
v0_2_0: spec
	<version: '0.2.0'>
	
	spec
		for: #'common'
		do: [ 
			spec
				baseline: 'OSSubprocess'
					with: [ spec repository: 'github://marianopeck/OSSubprocess:v0.2.0/repository' ];
				import: 'OSSubprocess' ].