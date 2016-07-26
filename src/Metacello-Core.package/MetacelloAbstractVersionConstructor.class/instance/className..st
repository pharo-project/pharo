api
className: aString
    "Define className field of a project spec (MetacelloMCProjectSpec).
	
		spec project: 'CoolBrowser' with: [
			spec className: 'ConfigurationOfCoolBrowser'. ].

	The className field is OPTIONAL in the project spec. If omitted, the className will be created by prepending 'ConfigurationOf' to the project name.
	 "

    self root className: aString constructor: self