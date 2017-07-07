actions
loadIceberg

	Metacello new
		baseline: 'Iceberg';
		repository: 'github://pharo-vcs/iceberg:v0.5.1';
		load.
	(Smalltalk classNamed: #Iceberg) enableMetacelloIntegration: true.
