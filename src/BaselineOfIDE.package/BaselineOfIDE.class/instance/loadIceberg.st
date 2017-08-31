actions
loadIceberg

	Metacello new
		baseline: 'Iceberg';
		repository: 'github://pharo-vcs/iceberg:v0.5.7';
		load.
	(Smalltalk classNamed: #Iceberg) enableMetacelloIntegration: true.
	(Smalltalk classNamed: #Iceberg) showSystemRepositories: true.
