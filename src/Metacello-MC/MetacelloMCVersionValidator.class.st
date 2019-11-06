"
Performs configuration validation.

For programmatically decoding reason codes use:

  MetacellMCVersionValidator fullDescriptionForReasonCode: <reasonCode>

Warning reason codes:

	#notDevelopmentVersion			- the symbolic version #development refers to a non-development literal version.
	#loadWarning 						- Warning signalled during load [load validation].
	#onlyBaselineVersion 				- one or more baseline versions have been defined, but no non-baseline versions are defined.
	#stableDevelopmentVersion		- a version whose blessing is #development has been declared as a #stable version

Critical Warning reason codes:

	#duplicateVersionDefinitions 		         - there are multiple pragma methods specifying the same version
	#loadDeprecation					         - deprecation warning signalled while loading configuration [load validation]
	#missingRecommendedProjectSpecField - missing recommended fields in project reference (versionString). The versionString should be specified so that #bleedingEdge loads will be predictable and repeatable
	#noLoadableVersions 				         - no non #baseline versions defined in configuration
	#noTests 							         - no test cases defined in loaded configuration [load validation]
	#noVersionSpecified 				         - no version defined for the project reference or package. The version specified in the baseline or the latest version of the project or package in the repository will be used.
	#packageNameMismatch 			         - the name in the packageSpec does not match the name of the mcz file
	#projectClassNameFileMismatch 	         - the class name of the configuration does not match the mcz file containing the configuration
	#testDeprecation 					         - deprecation warning signalled while running configuration tests [load validation]

Error reason codes:

	#cannotResolveVersion 			- the version (project reference or symbolic version) was not found in the specified configuration
	#duplicateNames 					- multiple independent definitions for an entity with same name (project, package, or group)
	#incompleteProjectSpec 			- missing required fields in project reference (className and/or repository)
	#incorrectVersionString 			- the version declared in pragma doesn't match version in versionSpec
	#invalidDoItSelector 				- doit select must be a Symbol
	#invalidVersionString 				- versionString must be a String
	#loadError 							- error occured while loading configuration [load validation]
	#missingVersionImport 			- version specified in import pragma not defined in configuration
	#noVersionsDefined 				- no usable baseline or version defined in configuration ... configuration cannot be loaded
	#projectCreationError 				- error occured while resolving project reference
	#shadowedNames 					- name duplication between packages and projects
	#testFailures						- test failures while running tests [load validation]
	#versionCompositionError 			- error while creating versionSpec from pragmas



"
Class {
	#name : #MetacelloMCVersionValidator,
	#superclass : #MetacelloVersionValidator,
	#category : #'Metacello-MC-Validation'
}

{ #category : #private }
MetacelloMCVersionValidator class >> populateReasonCodeDescriptions [
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
]

{ #category : #'instance creation' }
MetacelloMCVersionValidator class >> validateConfigurationLoad: configurationClass [
	^ ((self new)
		configurationClass: configurationClass;
		yourself) validateProjectLoad
]

{ #category : #'instance creation' }
MetacelloMCVersionValidator class >> validateConfigurationLoad: configurationClass version: versionString [
	^self validateConfigurationLoad: configurationClass version: versionString loads: #()
]

{ #category : #'instance creation' }
MetacelloMCVersionValidator class >> validateConfigurationLoad: configurationClass version: versionString loads: loadList [
	^ ((self new)
		configurationClass: configurationClass;
		yourself) validateProjectVersionLoad: versionString loads: loadList
]

{ #category : #private }
MetacelloMCVersionValidator >> criticalWarningReasonCodes [
	^ super criticalWarningReasonCodes
		, #(#noLoadableVersions #noTests #testDeprecation #loadDeprecation #noVersionSpecified #'missingRecommendedProjectSpecField' )
]

{ #category : #private }
MetacelloMCVersionValidator >> errorReasonCodes [
	^ super errorReasonCodes, #(#loadError #testFailures )
]

{ #category : #validation }
MetacelloMCVersionValidator >> validateBaselineVersionSpec: versionSpec [
    | projectNames packageNames groupNames versionMessage |
    self
        validateDoIts: versionSpec
        versionString: versionSpec versionString
        errorMessage: ' version ' , versionSpec versionString printString.
    projectNames := Set new.
    packageNames := Set new.
    groupNames := Set new.
    versionMessage := ' in version ' , versionSpec versionString printString.
    versionSpec
        projectDo: [ :projectSpec | 
            projectSpec resolveProjectSpec className == nil
                ifTrue: [ 
                    self
                        recordValidationError:
                            'Missing required field (className:) for project reference ' , projectSpec name printString , ' in version '
                                , versionSpec versionString printString
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'incompleteProjectSpec' ].
            projectSpec resolveProjectSpec versionString == nil
                ifTrue: [ 
                    self
                        recordValidationCriticalWarning:
                            'Missing recommended field (versionString:) for project reference ' , projectSpec name printString , ' in version '
                                , versionSpec versionString printString
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'missingRecommendedProjectSpecField' ].
            projectSpec hasRepository
                ifTrue: [ 
                    (self recurse and: [ projectSpec versionString ~~ nil ])
                        ifTrue: [ 
                            | project |
                            projectSpec resolveProjectSpec ensureProjectLoaded.
                            project := self
                                validateProjectCreationFrom: projectSpec resolveProjectSpec projectClass
                                onError: [ :ex | 
                                    self
                                        recordValidationError: 'Error creating project reference: ' , ex description
                                        versionString: versionSpec versionString
                                        callSite: #'validateBaselineVersionSpec:'
                                        reasonCode: #'projectCreationError'.
                                    nil ].
                            project ~~ nil
                                ifTrue: [ 
                                    self validationReport
                                        addAll:
                                            (self class
                                                validateProject: project
                                                version: projectSpec versionString
                                                debug: self debug
                                                recurse: self recurse
                                                visited: self visited) ] ] ]
                ifFalse: [ 
                    self
                        recordValidationError:
                            'Missing required field (repository:) for project reference ' , projectSpec name printString , ' in version '
                                , versionSpec versionString printString
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'incompleteProjectSpec' ].
            self
                validateDoIts: projectSpec
                versionString: versionSpec versionString
                errorMessage: projectSpec name printString , versionMessage.
            (projectNames includes: projectSpec name)
                ifTrue: [ 
                    self
                        recordValidationError:
                            'Duplicate projects named' , projectSpec name printString , versionMessage
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'duplicateNames' ]
                ifFalse: [ projectNames add: projectSpec name ] ]
        packageDo: [ :packageSpec | 
            self
                validateDoIts: packageSpec
                versionString: versionSpec versionString
                errorMessage: packageSpec name printString , versionMessage.
            (packageNames includes: packageSpec name)
                ifTrue: [ 
                    self
                        recordValidationError:
                            'Duplicate packages named' , packageSpec name printString , versionMessage
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'duplicateNames' ]
                ifFalse: [ projectNames add: packageSpec name ] ]
        groupDo: [ :groupSpec | 
            (groupNames includes: groupSpec name)
                ifTrue: [ 
                    self
                        recordValidationError: 'Duplicate groups named' , groupSpec name printString , versionMessage
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'duplicateNames' ]
                ifFalse: [ projectNames add: groupSpec name ] ].
    (packageNames intersection: projectNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between packages and projects' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ].
    (groupNames intersection: projectNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between groups and projects' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ].
    (projectNames intersection: packageNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between projects and packages' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ].
    (groupNames intersection: packageNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between groups and packages' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ].
    (projectNames intersection: groupNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between projects and groups' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ].
    (packageNames intersection: groupNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between packages and groups' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ]
]

{ #category : #loading }
MetacelloMCVersionValidator >> validateCleanLoadAndTestsForVersion: version loads: loadList [
	| cleanLoad cleanTests |
	cleanTests := cleanLoad := false.
	[ 
	self validateVersionLoad: version loads: loadList.
	cleanLoad := true ]
		on: Error , Warning
		do: [ :ex | 
			(ex isKindOf: Error)
				ifTrue: [ 
					self
						recordValidationError:
							'Error while loading version ' , version versionString printString , ' in ' , self configurationClass name asString
								, ' ' , ex description
						versionString: version versionString
						callSite: #validateCleanLoadAndTestsForVersion:loads:
						reasonCode: #loadError ].
			(ex isKindOf: Warning)
				ifTrue: [ 
					(ex isKindOf: Deprecation)
						ifTrue: [ 
							self
								recordValidationCriticalWarning:
									'Deprecation while loading version ' , version versionString printString , ' in '
										, self configurationClass name asString , ' ' , ex description
								versionString: version versionString
								callSite: #validateCleanLoadAndTestsForVersion:loads:
								reasonCode: #loadDeprecation ]
						ifFalse: [ 
							self
								recordValidationWarning:
									'Warning while loading version ' , version versionString printString , ' in ' , self configurationClass name asString
										, ' ' , ex description
								versionString: version versionString
								callSite: #validateCleanLoadAndTestsForVersion:loads:
								reasonCode: #loadWarning.
							Smalltalk
								at: #UndeclaredVariableWarning
								ifPresent: [ :undeclaredWrning | 
									(ex isKindOf: undeclaredWrning)
										ifTrue: [ ex resume: true ] ].
							ex resume ] ] ].
	cleanLoad
		ifTrue: [ 
			cleanTests := [ self validateVersionTests: version ]
				on: Deprecation
				do: [ :ex | 
					| message |
					message := 'Deprecation warning while running tests for version ' , version versionString printString , ' in '
						, self configurationClass name asString , ' ' , ex description.	"Deprecation warning for release tests is the same as a test failure"
					self
						recordValidationCriticalWarning: message
						versionString: version versionString
						callSite: #validateCleanLoadAndTestsForVersion:loads:
						reasonCode: #testDeprecation.
					ex return: false ].
			cleanTests
				ifTrue: [ MetacelloCleanLoadAndTestsNotification signal: version ]
				ifFalse: [ MetacelloCleanLoadNotification signal: version ] ]
]

{ #category : #loading }
MetacelloMCVersionValidator >> validateProjectLoad [
	| issues project versions currentVersion collectedIssues |
	(issues := self validateProject select: [ :issue | issue isError ]) notEmpty
		ifTrue: [ ^ issues ].
	project := self configurationClass project.
	collectedIssues := issues.
	versions := project versions
		select: [ :version | 
			validationReport := nil.
			version blessing ~~ #broken
				and: [ 
					version blessing ~~ #baseline
						and: [ (issues := (self validateProject: project version: version versionString) select: [ :issue | issue isCritical ]) isEmpty ] ] ].
	validationReport := collectedIssues.
	versions isEmpty
		ifTrue: [ 
			self
				recordValidationCriticalWarning: 'No non #baseline versions  available  in ' , self configurationClass name asString
				callSite: #validateProjectLoad
				reasonCode: #noLoadableVersions.
			^ self validationReport ].
	(currentVersion := project currentVersion) ~~ nil
		ifTrue: [ 
			| index |
			index := versions indexOf: currentVersion.
			versions := versions copyFrom: index to: versions size ].
	versions do: [ :version | self validateCleanLoadAndTestsForVersion: version loads: #('ALL') ].
	^ self validationReport
]

{ #category : #loading }
MetacelloMCVersionValidator >> validateProjectVersionLoad: versionString loads: loadList [
	| issues project version |
	(issues := (self validateProjectVersion: versionString) select: [ :issue | issue isError ]) notEmpty
		ifTrue: [ ^ issues ].
	project := self configurationClass project.
	version := project
		version: versionString
		ifAbsent: [ 
			self
				recordValidationError: 'Version ' , versionString printString , ' does not exist.'
				callSite: #validateProjectVersionLoad:loads:
				reasonCode: #cannotResolveVersion.
			^ self validationReport ].
	version blessing = #broken
		ifTrue: [ self error: 'The specified version is #broken' ].
	self validateCleanLoadAndTestsForVersion: version loads: loadList.
	^ self validationReport

]

{ #category : #loading }
MetacelloMCVersionValidator >> validateVersionLoad: version loads: loadList [
	| list |
	list := loadList asOrderedCollection.
	list isEmpty
		ifTrue: [ list add: 'default' ].
	(version groups includes: 'Tests')
		ifTrue: [ list add: 'Tests' ].
	version load: list
]

{ #category : #private }
MetacelloMCVersionValidator >> validateVersionSpec: versionSpec [
  versionSpec blessing value == #'broken'
    ifTrue: [ ^ self ].
  versionSpec
    projectDo: [ :projectSpec | 
      | referencedProjectSpec |
      projectSpec versionString == nil
        ifTrue: [ 
          self
            recordValidationCriticalWarning:
              'No version specified for the project reference ' , projectSpec name printString
                , ' in version ' , versionSpec versionString printString
            versionString: versionSpec versionString
            callSite: #'validateVersionSpec:'
            reasonCode: #'noVersionSpecified' ].
      referencedProjectSpec := projectSpec referencedSpec.
      versionSpec blessing value == #'baseline'
        ifTrue: [ 
          referencedProjectSpec hasRepository
            ifTrue: [ 
              (referencedProjectSpec file beginsWith: referencedProjectSpec className)
                ifFalse: [ 
                  self
                    recordValidationCriticalWarning:
                      'The class name of project ' , referencedProjectSpec className printString
                        , ' does not match the file name '
                        , referencedProjectSpec file printString
                        , ' in version ' , versionSpec versionString printString
                    versionString: versionSpec versionString
                    callSite: #'validateVersionSpec:'
                    reasonCode: #'projectClassNameFileMismatch' ] ]
            ifFalse: [ 
              self
                recordValidationError:
                  'The specification for the project reference ' , projectSpec name printString
                    , ' in version ' , versionSpec versionString printString
                    , ' is missing the required repository field'
                versionString: versionSpec versionString
                callSite: #'validateVersionSpec:'
                reasonCode: #'incompleteProjectSpec' ] ].
      referencedProjectSpec ensureProjectLoaded
        ifTrue: [ 
          [ projectSpec version ]
            on: MetacelloVersionDoesNotExistError
            do: [ :ex | 
              | explanation |
              explanation := projectSpec versionString == nil
                ifTrue: [ 'the default version' ]
                ifFalse: [ 'version ' , projectSpec versionString printString ].
              self
                recordValidationError:
                  'Cannot resolve ' , explanation , ' for the project reference '
                    , projectSpec name printString , ' in version '
                    , versionSpec versionString printString
                versionString: versionSpec versionString
                callSite: #'validateVersionSpec:'
                reasonCode: #'cannotResolveVersion' ] ] ]
    packageDo: [ :packageSpec | 
      (packageSpec file beginsWith: packageSpec name)
        ifFalse: [ 
          self
            recordValidationCriticalWarning:
              'The name of package ' , packageSpec name printString
                , ' does not match the file name '
                , packageSpec file printString , ' in version '
                , versionSpec versionString printString
            versionString: versionSpec versionString
            callSite: #'validateVersionSpec:'
            reasonCode: #'packageNameMismatch' ].
      packageSpec file = packageSpec name
        ifTrue: [ 
          self
            recordValidationCriticalWarning:
              'No version specified for the package ' , packageSpec name printString
                , ' in version ' , versionSpec versionString printString
            versionString: versionSpec versionString
            callSite: #'validateVersionSpec:'
            reasonCode: #'noVersionSpecified' ] ]
    groupDo: [ :ignored |  ].
  self validateBaselineVersionSpec: versionSpec
]

{ #category : #private }
MetacelloMCVersionValidator >> validateVersionSpecForSymbolicVersion: versionSpec symbolicVersion: symbolicVersionString [
	| blessing |
	versionSpec blessing value == #broken
		ifTrue: [ ^ self ].
	blessing := versionSpec blessing value.
	(symbolicVersionString == #development and: [ blessing ~~ #development ])
		ifTrue: [ 
			self
				recordValidationWarning:
					'Symbolic version ' , symbolicVersionString printString , ' refers to a version' , versionSpec versionString printString
						, ' whose blessing ' , blessing printString , ' is not #development'
				versionString: versionSpec versionString
				callSite: #validateVersionSpecForSymbolicVersion:symbolicVersion:
				reasonCode: #notDevelopmentVersion ]
]

{ #category : #loading }
MetacelloMCVersionValidator >> validateVersionTests: version [
	| testCases cleanTests |
	testCases := IdentitySet new.
	cleanTests := true.
	version currentlyLoadedClassesInVersion
		do: [ :class | 
			| isTestCase |
			isTestCase := (class withAllSuperclasses collect: #name) includes: #TestCase.
			"do not use direct inheritance check to avoid direct dependency of Metacello on SUnit"
			(isTestCase and: [ class isAbstract not ])
				ifTrue: [ testCases add: class ] ].
	testCases
		do: [ :testCase | 
			| testResults |
			testResults := testCase suite run.
			testResults defects notEmpty
				ifTrue: [ 
					self
						recordValidationError:
							'Test failures in tests ' , testCase name asString , ' for ' , version versionString printString , ' in '
								, self configurationClass name asString , ' ' , testResults printString
						versionString: version versionString
						callSite: #validateVersionTests:
						reasonCode: #testFailures.
					cleanTests := false ] ].
	testCases isEmpty
		ifTrue: [ 
			self
				recordValidationCriticalWarning:
					'No test cases for ' , version versionString printString , ' in ' , self configurationClass name asString
				versionString: version versionString
				callSite: #validateVersionTests:
				reasonCode: #noTests.
			cleanTests := false ].
	^ cleanTests
]

{ #category : #private }
MetacelloMCVersionValidator >> warningReasonCodes [
	^ super warningReasonCodes, #(#loadWarning #notDevelopmentVersion #stableDevelopmentVersion)
]
