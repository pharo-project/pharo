baseline
baseline: spec
	<baseline>
	spec for: #common do: [ spec
		baseline: 'FileTree'
			with: [ spec
				repository: 'github://npasserini/filetree:pharo5.0_dev/repository';
				loads: 'Git' ];
		package: 'Iceberg'
			with: [ spec requires: #('FileTree') ];
		package: 'Iceberg-Tests'
			with: [ spec requires: #('default') ];
		group: 'default'
			with: #('Iceberg');
		group: 'tests'
			with: #('Iceberg-Tests')
	]