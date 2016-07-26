instance creation
classic
    "set the options such that the load performed will be identical to the classic ConfigurationOf load:
	(ConfigurationOfExample project version: '1.0') load
	
	#classic forces Metacello to look at image state to determine which version of a project is loaded instead of using
		the registry to tell us explicitly which version of a project is loaded .. image state is not PRECISE"

    "useCurrentVersion is a 'private' option for enforcing classic rules, so it's not part of scripting api"

    ^ self new
        onUpgrade: [ :ex | ex allow ];
        onConflict: [ :ex | ex allow ];
        addStatement: #'useCurrentVersion:' args: {true};
        yourself