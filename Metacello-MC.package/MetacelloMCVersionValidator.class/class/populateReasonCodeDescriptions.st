private
populateReasonCodeDescriptions
    "update MetacelloMCVersionValidator class comment to include any changes to descriptions"

    | dict |
    dict := super populateReasonCodeDescriptions.
    dict
        at: #'notDevelopmentVersion'
            put: 'the symbolic version #development refers to a non-development literal version.';
        at: #'loadWarning' put: 'Warning signalled during load [load validation].';
        at: #'stableDevelopmentVersion'
            put: 'a version whose blessing is #development has been declared as a #stable version.'.	"Warnings"
    dict
        at: #'loadDeprecation' put: 'deprecation warning signalled while loading configuration [load validation].';
        at: #'missingRecommendedProjectSpecField'
            put:
                'missing recommended fields in project reference (versionString). The versionString should be specified so that #bleedingEdge loads will be predictable and repeatable.';
        at: #'noLoadableVersions' put: 'no non #baseline versions defined in configuration.';
        at: #'noTests' put: 'no test cases defined in loaded configuration [load validation].';
        at: #'noVersionSpecified'
            put:
                'no version defined for the project reference or package. The version specified in the baseline or the latest version of the project or package in the repository will be used.';
        at: #'testDeprecation'
            put: 'deprecation warning signalled while running configuration tests [load validation].'.	"Critical Warnings"
    dict
        at: #'loadError' put: 'error occured while loading configuration [load validation].';
        at: #'testFailures' put: 'test failures while running tests [load validation].'.	"Errors"
    ^ dict