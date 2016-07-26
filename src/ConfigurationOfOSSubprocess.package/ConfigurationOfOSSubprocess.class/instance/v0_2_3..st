versions
v0_2_3: spec
	<version: '0.2.3'>
	
	spec
		for: #'pharo'
		do: [ 
			spec
				baseline: 'OSSubprocess'
					with: [ spec repository: 'github://marianopeck/OSSubprocess:v0.2.3/repository' ];
				import: 'OSSubprocess' ].