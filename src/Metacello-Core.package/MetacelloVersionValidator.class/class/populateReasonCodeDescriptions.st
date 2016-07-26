private
populateReasonCodeDescriptions
    "update MetacelloMCVersionValidator class comment to include any changes to descriptions"

    | dict |
    dict := Dictionary new.
    dict
        at: #'onlyBaselineVersion'
        put: 'one or more baseline versions have been defined, but no non-baseline versions are defined.'.
    dict
        at: #'duplicateVersionDefinitions' put: 'there are multiple pragma methods specifying the same version.';
        at: #'packageNameMismatch' put: 'the name in the packageSpec does not match the name of the mcz file.';
        at: #'projectClassNameFileMismatch'
            put: 'the class name of the configuration does not match the mcz file containing the configuration.';
        at: #'cannotResolveVersion'
            put: 'the version (project reference or symbolic version) was not found in the specified configuration.'.
    dict
        at: #'duplicateNames'
            put: 'multiple independent definitions for an entity with same name (project, package, or group).';
        at: #'incompleteProjectSpec' put: 'missing required fields in project reference (repository, className).';
        at: #'incorrectVersionString' put: 'the version declared in pragma doesn''t match version in versionSpec.';
        at: #'invalidDoItSelector' put: 'doit select must be a Symbol.';
        at: #'invalidVersionString' put: 'versionString must be a String.';
        at: #'missingVersionImport' put: 'version specified in import pragma not defined in configuration.';
        at: #'noVersionsDefined'
            put: 'no usable baseline or version defined in configuration ... configuration cannot be loaded.';
        at: #'projectCreationError' put: 'error occured while resolving project reference.';
        at: #'versionCreationError'
            put:
                'error occured while resolving version specification, error will be thrown if an attempt is made to use the version.';
        at: #'shadowedNames' put: 'name duplication between packages and projects.';
        at: #'versionCompositionError' put: 'error while creating versionSpec from pragmas.'.
    ^ dict