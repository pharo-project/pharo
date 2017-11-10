Class {
	#name : #MetacelloVersionValidator,
	#superclass : #MetacelloAbstractConstructor,
	#instVars : [
		'configurationClass',
		'exludededValidations',
		'validationReport',
		'recurse',
		'debug',
		'visited'
	],
	#classInstVars : [
		'reasonCodeDescriptions'
	],
	#category : #Metacello-Core-Validation
}

{ #category : #accessing }
MetacelloVersionValidator class >> descriptionForReasonCode: reasonCode [
	^ self reasonCodeDescriptions at: reasonCode ifAbsent: [ self error: 'Unknown reasonCode: ' , reasonCode printString ]
]

{ #category : #private }
MetacelloVersionValidator class >> populateReasonCodeDescriptions [
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
]

{ #category : #accessing }
MetacelloVersionValidator class >> reasonCodeDescriptions [

	reasonCodeDescriptions ifNil: [ reasonCodeDescriptions := self populateReasonCodeDescriptions ].
	^reasonCodeDescriptions
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateBaseline: baselineClass [
  ^ self validateBaseline: baselineClass debug: #() recurse: false
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateBaseline: baselineClass debug: debugList recurse: aBool [
  ^ (self new
    configurationClass: baselineClass;
    debug: debugList;
    recurse: aBool;
    yourself) validateBaselineOf
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateBaseline: baselineClass recurse: aBool [
  ^ self validateBaseline: baselineClass debug: #() recurse: aBool
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateConfiguration: configurationClass [

	^self validateConfiguration: configurationClass debug: #() recurse: false
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateConfiguration: configurationClass debug: debugList recurse: aBool [
	^ ((self new)
		configurationClass: configurationClass;
		debug: debugList;
		recurse: aBool;
		yourself) validate
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateConfiguration: configurationClass recurse: aBool [

	^self validateConfiguration: configurationClass debug: #() recurse: aBool
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateProject: aMetacelloProject [

	^self validateProject: aMetacelloProject debug: #() recurse: false
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateProject: aMetacelloProject debug: debugList recurse: aBool [
	^ ((self new)
		recurse: aBool;
		debug: debugList;
		configurationClass: aMetacelloProject configuration class;
		yourself) validateProject: aMetacelloProject
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateProject: aMetacelloProject recurse: aBool [

	^self validateProject: aMetacelloProject debug: #() recurse: aBool
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateProject: aMetacelloProject version: versionString [

	^self validateProject: aMetacelloProject version: versionString debug: #() recurse: false
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateProject: aMetacelloProject version: versionString debug: debugList [

	^self validateProject: aMetacelloProject version: versionString debug: debugList recurse: false
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateProject: aMetacelloProject version: versionString debug: debugList recurse: aBool [

	^ ((self new)
		configurationClass: aMetacelloProject configuration class;
		debug: debugList;
		recurse: aBool;
		yourself) validateProject: aMetacelloProject version: versionString
]

{ #category : #'instance creation' }
MetacelloVersionValidator class >> validateProject: aMetacelloProject version: versionString debug: debugList recurse: aBool visited: visitied [

	^(self new
		configurationClass: aMetacelloProject configuration class;
		debug: debugList;
		recurse: aBool;
		visited: visitied;
		yourself) validateProject: aMetacelloProject version: versionString
]

{ #category : #accessing }
MetacelloVersionValidator >> configurationClass [
	^ configurationClass
]

{ #category : #accessing }
MetacelloVersionValidator >> configurationClass: anObject [
	configurationClass := anObject
]

{ #category : #private }
MetacelloVersionValidator >> criticalWarningReasonCodes [
    ^ #(#'packageNameMismatch' #'projectClassNameFileMismatch' #'duplicateVersionDefinitions')
]

{ #category : #accessing }
MetacelloVersionValidator >> debug [

	debug == nil ifTrue: [ debug := #() ].
	^ debug
]

{ #category : #accessing }
MetacelloVersionValidator >> debug: aCollection [
	"Any combination of: #error, #criticalWarning, #warning"

	debug := aCollection
]

{ #category : #private }
MetacelloVersionValidator >> errorReasonCodes [
    ^ #(#'duplicateNames' #'shadowedNames' #'invalidDoItSelector' #'invalidVersionString' #'missingVersionImport' #'projectCreationError' #'noVersionsDefined' #'cannotResolveVersion' #'incompleteProjectSpec' #'incorrectVersionString' #'versionCompositionError' #'versionCreationError')
]

{ #category : #accessing }
MetacelloVersionValidator >> exludededValidations [

	exludededValidations == nil ifTrue: [ exludededValidations := self extractExcludedValidations ].
	^exludededValidations
]

{ #category : #'pragma extraction' }
MetacelloVersionValidator >> extractExcludedValidations [

	| exclusionDict |
	exclusionDict := Dictionary new.
	(Pragma 
		allNamed: #excludedValidationIssues:
		in: self configurationClass) do: [:pragma | | exclusions |
			exclusions := pragma argumentAt: 1.
			1 to: exclusions size by: 2 do: [:index |
			exclusionDict 
				at: (exclusions at: index) 
				put: (exclusions at: index + 1) ]].
	^exclusionDict
]

{ #category : #accessing }
MetacelloVersionValidator >> recordValidationCriticalWarning: aString callSite: callSite reasonCode: aSymbol [

	^self recordValidationCriticalWarning: aString versionString: nil callSite: callSite reasonCode: aSymbol
]

{ #category : #accessing }
MetacelloVersionValidator >> recordValidationCriticalWarning: aString versionString: versionString callSite: callSite reasonCode: aSymbol [
	"reasonCodes:
		#packageNameMismatch
		#projectClassNameFileMismatch
		#duplicateVersionDefinitions
		#duplicateAttributeBlocks
	"

	(self criticalWarningReasonCodes includes: aSymbol)
		ifFalse: [ self error: 'Unknown critical warning reason code' ].
	((self exludededValidations at: versionString ifAbsent: [ #() ]) includes: aSymbol)
		ifTrue: [ ^self ].
	(self debug includes: #criticalWarning) ifTrue: [ self halt: 'Debug triggered for critical warning: ', aString ].
	self validationReport
		add:
			(MetacelloValidationCriticalWarning
				configurationClass:
					(self recurse
						ifTrue: [ self configurationClass ]
						ifFalse: [ nil ])
				reasonCode: aSymbol
				callSite: callSite
				explanation: aString)
]

{ #category : #accessing }
MetacelloVersionValidator >> recordValidationError: aString callSite: callSite reasonCode: aSymbol [

	^self recordValidationError: aString versionString: nil callSite: callSite reasonCode: aSymbol
]

{ #category : #accessing }
MetacelloVersionValidator >> recordValidationError: aString versionString: versionString callSite: callSite reasonCode: aSymbol [
	"reasonCodes:
		#noVersionForSymbolicVersion
		#duplicateNames
		#shadowedNames
		#invalidDoItSelector
		#invalidVersionString
		#missingVersionImport
		#projectCreationError
		#noVersionsDefined
		#cannotResolveVersion
		#incompleteProjectSpec
		#incorrectVersionString
	"

	(self errorReasonCodes includes: aSymbol)
		ifFalse: [ self error: 'Unknown error reason code' ].
	((self exludededValidations at: versionString ifAbsent: [ #() ]) includes: aSymbol)
		ifTrue: [ ^self ].
	(self debug includes: #error) ifTrue: [ self halt: 'Debug triggered for error: ', aString ].
	self validationReport
		add:
			(MetacelloValidationError
				configurationClass:
					(self recurse
						ifTrue: [ self configurationClass ]
						ifFalse: [ nil ])
				reasonCode: aSymbol
				callSite: callSite
				explanation: aString)
]

{ #category : #accessing }
MetacelloVersionValidator >> recordValidationWarning: aString callSite: callSite reasonCode: aSymbol [

				
	^self recordValidationWarning: aString versionString: nil callSite: callSite reasonCode: aSymbol
]

{ #category : #accessing }
MetacelloVersionValidator >> recordValidationWarning: aString versionString: versionString callSite: callSite reasonCode: aSymbol [
	"reasonCodes:
		#onlyBaselineVersion
		#noVersionSpecified
	"

	(self warningReasonCodes includes: aSymbol)
		ifFalse: [ self error: 'Unknown warning reason code' ].
	((self exludededValidations at: versionString ifAbsent: [ #() ]) includes: aSymbol)
		ifTrue: [ ^self ].
	(self debug includes: #warning) ifTrue: [ self halt: 'Debug triggered for critical warning: ', aString ].
	self validationReport
		add:
			(MetacelloValidationWarning
				configurationClass:
					(self recurse
						ifTrue: [ self configurationClass ]
						ifFalse: [ nil ])
				reasonCode: aSymbol
				callSite: callSite
				explanation: aString)
]

{ #category : #accessing }
MetacelloVersionValidator >> recurse [
	recurse == nil ifTrue: [ recurse := false ].
	^ recurse
]

{ #category : #accessing }
MetacelloVersionValidator >> recurse: anObject [
	recurse := anObject
]

{ #category : #validation }
MetacelloVersionValidator >> validate [
	"Issue 5: 
		[]no shadowing of names across project/package/group boundaries
	 Issue 6: 
		[]package version supplied for each package
		[]no released (stable/bleedingEdge) versions (only development version)
		package spec used instead of project spec in baseline (ConfigurationOfPharo-DaleHenrichs.50)
	Issue 20:
		[]pre and post load doits must be symbols, not blocks
	Others:
		[]symbolic versions can be resolved.
		[]Ensure that versions are Strings and symbolic versions are symbols.
	 	[]Ensure that imported versions are defined."

	self validatePragmas.
	^self validateProject
]

{ #category : #validation }
MetacelloVersionValidator >> validateBaselineOf [
  self validatePragmas.
  ^ self validateBaselineProject
]

{ #category : #private }
MetacelloVersionValidator >> validateBaselineProject [
  | project |
  project := self
    validateProjectCreationFrom: self configurationClass
    onError: [ :ex | 
      self
        recordValidationError:
          'Error creating project: ' , ex description
            , ' to reproduce evalutate the following: '''
            , self configurationClass name asString , ' project'''
        callSite: #'validateBaselineProject'
        reasonCode: #'projectCreationError'.
      ^ self validationReport ].
  ^ self validateBaselineProject: project
]

{ #category : #validation }
MetacelloVersionValidator >> validateBaselineProject: project [
  | version spec |
  (self visited includes: project configuration class)
    ifTrue: [ ^ self validationReport ].
  self visited add: project configuration class.
  version := project version.
  spec := version spec.
  [ self validateBaselineVersionSpec: spec ]
    on: Error
    do: [ :ex | 
      self
        recordValidationError:
          'Error composing version ' , version versionString printString , ': '
            , ex description
        versionString: version versionString
        callSite: #'validateBaselineProject:'
        reasonCode: #'versionCompositionError' ].
  ^ self validationReport
]

{ #category : #private }
MetacelloVersionValidator >> validateBaselineVersionSpec: versionSpec [

	self subclassResponsibility
]

{ #category : #private }
MetacelloVersionValidator >> validateDoIts: aSpec versionString: versionString errorMessage: errorMessage [
	| ar |
	ar := {
		#preLoadDoIt.	'preLoadDoIt selector for '.
		#postLoadDoIt.	'postLoadDoIt selector for '.
	}.
	1 to: ar size by: 2 do: [ :i | 
		| selector |
		selector := (aSpec perform: (ar at: i)) value.
		selector ~~ nil
			ifTrue: [ 
				selector isSymbol
					ifFalse: [ 
						self
							recordValidationError: (ar at: i + 1) , errorMessage , ' is not a symbol'
							versionString: versionString
							callSite: #validateDoIts:versionString:errorMessage:
							reasonCode: #invalidDoItSelector ] ] ]
]

{ #category : #private }
MetacelloVersionValidator >> validatePragmas [
    "Ensure that versions are Strings and symbolic versions are symbols.
	 Ensure that imported versions are defined"

    | versionPragmas versionImportPragmas symbolicVersionPragmas versionStrings |
    versionPragmas := self extractVersionPragmas.
    versionImportPragmas := self extractVersionImportPragmas.
    symbolicVersionPragmas := self extractSymbolicVersionPragmas.
    versionStrings := Set new.
    versionPragmas keys , versionImportPragmas keys , symbolicVersionPragmas keys
        do: [ :versionString | 
            (versionStrings includes: versionString)
                ifTrue: [ 
                    self
                        recordValidationCriticalWarning:
                            'version ' , versionString printString , ' is defined multiple times.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'duplicateVersionDefinitions' ]
                ifFalse: [ versionStrings add: versionString ] ].
    versionPragmas
        keysAndValuesDo: [ :versionString :pragmaList | 
            versionString isString
                ifFalse: [ 
                    self
                        recordValidationError: 'version ' , versionString printString , ' is not a String.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'invalidVersionString' ].
            pragmaList size > 1
                ifTrue: [ 
                    self
                        recordValidationCriticalWarning:
                            'version ' , versionString printString , ' is defined multiple times.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'duplicateVersionDefinitions' ] ].
    symbolicVersionPragmas
        keysAndValuesDo: [ :versionString :pragmaList | 
            versionString isSymbol
                ifFalse: [ 
                    self
                        recordValidationError: 'symbolic version ' , versionString printString , ' is not a Symbol.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'invalidVersionString' ].
            pragmaList size > 1
                ifTrue: [ 
                    self
                        recordValidationCriticalWarning:
                            'version ' , versionString printString , ' is defined multiple times.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'duplicateVersionDefinitions' ] ].
    versionImportPragmas
        keysAndValuesDo: [ :versionString :pragmaList | 
            versionString isString
                ifFalse: [ 
                    self
                        recordValidationError: 'version ' , versionString printString , ' is not a String.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'invalidVersionString' ].
            pragmaList size > 1
                ifTrue: [ 
                    self
                        recordValidationCriticalWarning:
                            'version ' , versionString printString , ' is defined multiple times.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'duplicateVersionDefinitions' ].
            pragmaList
                do: [ :versionImportPragma | 
                    (versionImportPragma arguments at: 2)
                        do: [ :importedVersionString | 
                            (versionStrings includes: importedVersionString)
                                ifFalse: [ 
                                    self
                                        recordValidationError:
                                            'version ' , importedVersionString printString , ' referenced in import list of version ' , versionString printString
                                                , ' has not been defined.'
                                        versionString: versionString
                                        callSite: #'validatePragmas'
                                        reasonCode: #'missingVersionImport' ] ] ] ]
]

{ #category : #private }
MetacelloVersionValidator >> validateProject [
	| project |
	project := self
		validateProjectCreationFrom: self configurationClass
		onError: [ :ex | 
			self
				recordValidationError:
					'Error creating project: ' , ex description , ' to reproduce evalutate the following: '''
						, self configurationClass name asString , ' project'''
				callSite: #validateProject
				reasonCode: #projectCreationError.
			^ self validationReport ].
	^ self validateProject: project
]

{ #category : #validation }
MetacelloVersionValidator >> validateProject: project [
	| hasNoBaseline hasNoVersion |
	(self visited includes: project configuration class)
		ifTrue: [ ^ self validationReport ].
	self visited add: project configuration class.
	hasNoBaseline := hasNoVersion := true.
	project versions
		do: [ :version | 
			| spec |
			spec := version spec.
			[ 
			spec blessing value == #baseline
				ifTrue: [ 
					hasNoBaseline := false.
					self validateBaselineVersionSpec: spec ]
				ifFalse: [ 
					hasNoVersion := false.
					self validateVersionSpec: spec ] ]
				on: Error
				do: [ :ex | 
					self
						recordValidationError: 'Error composing version ' , version versionString printString , ': ' , ex description
						versionString: version versionString
						callSite: #validateProject:
						reasonCode: #versionCompositionError ] ].
	project symbolicVersionMap keys
		do: [ :symbolicVersion | 
			| version |
			version := [ project version: symbolicVersion ]
				on: Error , MetacelloSymbolicVersionNotDefinedError
				do: [ :ex | 
					"MetacelloSymbolicVersionNotDefinedError are explicitly not defined, so are not validation errors"
					(ex isKindOf: MetacelloSymbolicVersionNotDefinedError)
						ifFalse: [ 
							self
								recordValidationError:
									'symbolic version ' , symbolicVersion printString , ' does not resolve to a literal version.'
								callSite: #validatePragmas
								reasonCode: #cannotResolveVersion ].
					ex return: nil ].
			version ~~ nil
				ifTrue: [ self validateVersionSpecForSymbolicVersion: version spec symbolicVersion: symbolicVersion ] ].
	hasNoVersion
		ifTrue: [ 
			hasNoBaseline
				ifTrue: [ 
					self
						recordValidationError: 'No usable baseline or versions defined.'
						callSite: #validatePragmas
						reasonCode: #noVersionsDefined ]
				ifFalse: [ 
					self
						recordValidationWarning: 'Only baseline defined (no version defined).'
						callSite: #validatePragmas
						reasonCode: #onlyBaselineVersion ] ].
	^ self validationReport
]

{ #category : #validation }
MetacelloVersionValidator >> validateProject: project version: versionString [
	| version spec |
	(self visited includes: project configuration class)
		ifTrue: [ ^ self validationReport ].
	self visited add: project configuration class.
	[ version := project version: versionString ]
		on: Error
		do: [ :ex | 
			self
				recordValidationError: 'Could not resolve version ' , versionString printString , ' due to error: ' , ex description
				versionString: versionString
				callSite: #validateProject:version:
				reasonCode: #cannotResolveVersion.
			^ self validationReport ].
	spec := version spec.
	[ 
	spec blessing value == #baseline
		ifTrue: [ self validateBaselineVersionSpec: spec ]
		ifFalse: [ self validateVersionSpec: spec ] ]
		on: Error
		do: [ :ex | 
			self
				recordValidationError: 'Error composing version ' , version versionString printString , ': ' , ex description
				versionString: version versionString
				callSite: #validateProject:version:
				reasonCode: #versionCompositionError ].
	^ self validationReport
]

{ #category : #private }
MetacelloVersionValidator >> validateProjectCreationFrom: aConfigurationClass onError: aBlock [
    [ ^ aConfigurationClass project ]
        on: Error , MetacelloValidationNotification , MetacelloErrorInProjectConstructionNotification
        do: [ :ex | 
            (ex isKindOf: MetacelloValidationNotification)
                ifTrue: [ 
                    self validationReport add: ex issue.
                    ex resume ].
            (ex isKindOf: MetacelloErrorInProjectConstructionNotification)
                ifTrue: [ 
                    self
                        recordValidationError:
                            'Error creating version: ' , ex versionString , ' error: ' , ex exception description
                                , ' to reproduce evalutate the following: ''[' , self configurationClass name asString
                                ,
                                    ' project ] on: MetacelloErrorInProjectConstructionNotification do: [:ex | ex resume: true ]'''
                        callSite: #'validateProjectCreationFrom:onError:'
                        reasonCode: #'versionCreationError'.
                    ex resume: false ].
            ^ aBlock value: ex ]
]

{ #category : #private }
MetacelloVersionValidator >> validateProjectVersion: versionString [
	| project |
	project := self
		validateProjectCreationFrom: self configurationClass
		onError: [ :ex | 
			self
				recordValidationError:
					'Error creating project: ' , ex description , ' to reproduce evalutate the following: '''
						, self configurationClass name asString , ' project'''
				callSite: #validateProject
				reasonCode: #projectCreationError.
			^ self validationReport ].
	^ self validateProject: project version: versionString
]

{ #category : #private }
MetacelloVersionValidator >> validateVersionSpec: versionSpec [

	self subclassResponsibility
]

{ #category : #private }
MetacelloVersionValidator >> validateVersionSpecForSymbolicVersion: versionSpec symbolicVersion: symbolicVersionString [

	self subclassResponsibility
]

{ #category : #accessing }
MetacelloVersionValidator >> validationReport [
	
	validationReport == nil ifTrue: [ validationReport := OrderedCollection new ].
	^ validationReport
]

{ #category : #accessing }
MetacelloVersionValidator >> validationReport: anObject [
	validationReport := anObject
]

{ #category : #accessing }
MetacelloVersionValidator >> visited [

	visited == nil ifTrue: [ visited := IdentitySet new ].
	^visited
]

{ #category : #accessing }
MetacelloVersionValidator >> visited: anIdentitySet [

	visited := anIdentitySet
]

{ #category : #private }
MetacelloVersionValidator >> warningReasonCodes [
	^ #(#onlyBaselineVersion )
]
