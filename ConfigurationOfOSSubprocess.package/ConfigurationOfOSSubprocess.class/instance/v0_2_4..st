versions
v0_2_4: spec
	<version: '0.2.4'>
	
	spec
		for: #'pharo'
		do: [ 
			spec
				baseline: 'OSSubprocess'
					with: [ spec repository: 'github://marianopeck/OSSubprocess:v0.2.4/repository' ];
				import: 'OSSubprocess' ].