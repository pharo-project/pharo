Class {
	#name : #MBCommandMetacelloTests,
	#superclass : #MBCommandTests,
	#category : #Versionner-Tests-Core-Commands
}

{ #category : #tests }
MBCommandMetacelloTests >> testAddConfigurationCmd [

	| versionString packageNames projectNames project version packages projects |
	self tearDownPackages addAll: #('ConfigurationOfMBFooTests' 'MBFooCommandTestA' 'MBFooCommandTestB'). "must be BEFORE the load"
	gofer version: 'MBFooCommandTestA-dkh.1'.
	gofer version: 'MBFooCommandTestB-dkh.1'.
	gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
	gofer load.
	versionString := '1.0'.
	packageNames := #('MBFooCommandTestA' 'MBFooCommandTestB').
	projectNames := #('ConfigurationOfMBFooTests').
	MBAddConfigurationCommand new
		executeAddBaselineForConfigurationNamed: self configurationName
		repositoryDescription: 'dictionary://', self repositoryName asString
		versionString: versionString
		packages: packageNames
		configurations: projectNames.
	project := (Smalltalk at: self configurationName asSymbol) project.
	version := project version: versionString, '-baseline'.
	packages := version packages.
	self assert: packages size == 2.
	packages do: [:pkg |
		self assert: (packageNames includes: pkg name)].
	projects := version projects.
	projects do: [:prj |
		self assert: (projectNames includes: prj name)].

]

{ #category : #tests }
MBCommandMetacelloTests >> testConfigurationChanges [
]

{ #category : #tests }
MBCommandMetacelloTests >> testRemoveVersionCmd01 [
	"Issue 117: Removing a version does not update the #development/#stable symbolic versions."

	| configName class configurationRoot configInfo versionInfo cmd |
	configName := 'ConfigurationOfMBFooIssue117'.
	self tearDownPackages addAll: {configName}.
	gofer version: 'ConfigurationOfMBFooIssue117-dkh.1'.
	gofer load.
	class := Smalltalk at: configName asSymbol.
	self assert: (class project version: #development) versionString = '1.0'.
	self assert: (class project version: #stable) versionString = '1.0'.
	configurationRoot := MBConfigurationRoot new.
	configurationRoot register.
	configInfo := configurationRoot configurationInfoFor: class.
	versionInfo := configInfo versions detect: [ :each | each versionString = '1.0' ].
	cmd := VSRemoveCommand target: versionInfo for: self.
	[ cmd execute ]
		valueSupplyingAnswers:
			{{'Are you sure you want to remove*'.
			true}}.
	self should: [ class project version: #development ] raise: MetacelloSymbolicVersionNotDefinedError.
	self should: [ class project version: #stable ] raise: MetacelloSymbolicVersionNotDefinedError.
	self should: [ class project version: '1.0' ] raise: MetacelloVersionDoesNotExistError
]

{ #category : #tests }
MBCommandMetacelloTests >> testRemoveVersionCmd02 [
	"Remove a bleedingEdge version that is imported ... should abort the operation"

	| configName class configurationRoot configInfo versionInfo cmd versionString |
	configName := 'ConfigurationOfMBFooIssue117'.
	self tearDownPackages addAll: {configName}.
	gofer version: 'ConfigurationOfMBFooIssue117-dkh.1'.
	gofer load.
	class := Smalltalk at: configName asSymbol.
	versionString := (class project version: #bleedingEdge) versionString.
	configurationRoot := MBConfigurationRoot new.
	configurationRoot register.
	configInfo := configurationRoot configurationInfoFor: class.
	versionInfo := configInfo baselines detect: [ :each | each versionString = versionString ].
	cmd := VSRemoveCommand target: versionInfo for: self.
	[ cmd execute ]
		valueSupplyingAnswers:
			{{'*is imported by another version*'.
			true}}.
	self assert: versionString = (class project version: #bleedingEdge) versionString
]

{ #category : #tests }
MBCommandMetacelloTests >> testRemoveVersionCmd03 [
	"Remove a bleedingEdge version with no imports"

	| configName class configurationRoot configInfo versionInfo cmd versionString |
	configName := 'ConfigurationOfMBFooIssue117'.
	self tearDownPackages addAll: {configName}.
	gofer version: 'ConfigurationOfMBFooIssue117-dkh.2'.
	gofer load.
	class := Smalltalk at: configName asSymbol.
	versionString := (class project version: #bleedingEdge) versionString.
	self assert: versionString = '2.0-baseline'.
	configurationRoot := MBConfigurationRoot new.
	configurationRoot register.
	configInfo := configurationRoot configurationInfoFor: class.
	versionInfo := configInfo baselines detect: [ :each | each versionString = versionString ].
	cmd := VSRemoveCommand target: versionInfo for: self.
	[ cmd execute ]
		valueSupplyingAnswers:
			{{'Are you sure you want to remove*'.
			true}}.
	self assert: (class project version: #bleedingEdge) versionString = '1.0-baseline'.
	self should: [ class project version: versionString ] raise: MetacelloVersionDoesNotExistError
]

{ #category : #tests }
MBCommandMetacelloTests >> testRemoveVersionCmd04 [
	"Remove a bleedingEdge version with no imports"

	| configName class configurationRoot configInfo versionInfo cmd versionString |
	configName := 'ConfigurationOfMBFooIssue117'.
	self tearDownPackages addAll: {configName}.
	gofer version: 'ConfigurationOfMBFooIssue117-dkh.3'.
	gofer load.
	class := Smalltalk at: configName asSymbol.
	versionString := (class project version: #bleedingEdge) versionString.
	self assert: versionString = '2.0-baseline'.
	configurationRoot := MBConfigurationRoot new.
	configurationRoot register.
	configInfo := configurationRoot configurationInfoFor: class.
	versionInfo := configInfo baselines detect: [ :each | each versionString = versionString ].
	cmd := VSRemoveCommand target: versionInfo for: self.
	[ cmd execute ]
		valueSupplyingAnswers:
			{{'Are you sure you want to remove*'.
			true}}.
	self should: [ class project version: #bleedingEdge ] raise: MetacelloSymbolicVersionNotDefinedError.
	self should: [ class project version: versionString ] raise: MetacelloVersionDoesNotExistError
]

{ #category : #tests }
MBCommandMetacelloTests >> testUsernamePassword [
	
	| cmd configInfo |
	gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
	gofer load.
	self tearDownPackages add: 'ConfigurationOfMBFooTests'.
	configInfo := (MBConfigurationInfo configurationClass: (Smalltalk at: #'ConfigurationOfMBFooTests'))
		configurationRoot: MBConfigurationRoot new.
	cmd := MBSaveConfigurationCommand target: configInfo for: self.
	[cmd username] valueSupplyingAnswer: 'ab'.
	[cmd password] valueSupplyingAnswer: 'foobar'.
	
	self assert: cmd username = 'ab'.
	self assert: cmd password = 'foobar'.
]
