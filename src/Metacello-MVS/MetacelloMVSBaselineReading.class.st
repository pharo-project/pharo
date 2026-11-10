"
I am what reading the baseline of one version gave: its replaces, the findings, and what it requires, which depends on the groups loaded from it.

baselineClass: the BaselineOf class the sandbox compiled, installed nowhere; newProject builds from it the Metacello project that Metacello records from.
versionSpec: the Metacello version spec of that class. referencesFor:into: expands groups with it, through the same walk Metacello records with (MetacelloVersionSpec>>#packageSpecsInLoadOrderFor:).
repositoryDescription: where the baseline was read from (repository:version/path), where its packages are.
references: what a load of the default groups requires. A reading told by hand (tests) has only these, whatever the groups.
replacements: project name -> repository string, from spec replace:with:.
request: the request of the first candidate that read me; my findings are about it.
findings: what reading the baseline found, the same whatever the groups.
"
Class {
	#name : 'MetacelloMVSBaselineReading',
	#superclass : 'Object',
	#instVars : [
		'references',
		'replacements',
		'versionSpec',
		'baselineClass',
		'repositoryDescription',
		'request',
		'findings'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSBaselineReading class >> references: aCollection [

	^ self new references: aCollection
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> add: aFinding [

	findings add: aFinding.
	^ aFinding
]

{ #category : 'groups' }
MetacelloMVSBaselineReading >> baselineClass [
	"The BaselineOf class the sandbox compiled, installed nowhere; nil when the reading was told"

	^ baselineClass
]

{ #category : 'groups' }
MetacelloMVSBaselineReading >> baselineClass: aClass [

	baselineClass := aClass.
	versionSpec := aClass version spec
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> error: aKind text: aString [

	^ self add: (MetacelloResolutionFinding error: aKind text: aString request: request)
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> findings [

	^ findings
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> hasErrors [

	^ findings anySatisfy: [ :each | each isError ]
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> initialize [

	super initialize.
	references := #(  ).
	replacements := Dictionary new.
	findings := OrderedCollection new
]

{ #category : 'groups' }
MetacelloMVSBaselineReading >> newProject [
	"A Metacello project of the baseline, whose packages come from where it was read: what Metacello records from"

	^ baselineClass new initalizeProjectWithRepositoryDescription: { repositoryDescription }
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> references [

	^ references
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> references: aCollection [

	references := aCollection asArray
]

{ #category : 'groups' }
MetacelloMVSBaselineReading >> referencesFor: aCollectionOfNames into: aFindingsCollector [
	"The project specs a load of the groups (or packages) aCollectionOfNames requires, the default ones when empty: what Metacello would record for that load, through includes and requires. A name the baseline does not declare (#missingGroup) is an error given to aFindingsCollector (anything that understands error:text:)"

	versionSpec ifNil: [ ^ references ].
	^ (([ versionSpec packageSpecsInLoadOrderFor: (aCollectionOfNames ifEmpty: [ versionSpec defaultPackageNames ]) ]
		    on: Error
		    do: [ :error |
				    aFindingsCollector error: #missingGroup text: error messageText.
				    ^ #(  ) ]) select: [ :each | each isProjectReferenceSpec ]) collect: [ :each | each projectReference copy ]
]

{ #category : 'groups' }
MetacelloMVSBaselineReading >> referencesForGroups: aCollectionOfNames includingDefault: aBoolean into: aFindingsCollector [
	"The projects a load of the groups aCollectionOfNames, plus the default ones when aBoolean, requires. Told readings answer their references whatever the groups"

	| names |
	versionSpec ifNil: [ ^ references ].
	names := aCollectionOfNames asOrderedCollection.
	aBoolean ifTrue: [ names addAll: versionSpec defaultPackageNames ].
	^ self referencesFor: names asSet asArray into: aFindingsCollector
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> replacements [

	^ replacements
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> replacements: aDictionary [

	replacements := aDictionary
]

{ #category : 'groups' }
MetacelloMVSBaselineReading >> repositoryDescription [
	"Where the baseline was read from, as repository:version/path: where its packages are"

	^ repositoryDescription
]

{ #category : 'groups' }
MetacelloMVSBaselineReading >> repositoryDescription: aString [

	repositoryDescription := aString
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> request: aProjectRequest [
	"The request of the first candidate that read me: the problems I find are about it"

	request := aProjectRequest
]

{ #category : 'groups' }
MetacelloMVSBaselineReading >> versionSpec [

	^ versionSpec
]

{ #category : 'groups' }
MetacelloMVSBaselineReading >> versionSpec: aMetacelloVersionSpec [

	versionSpec := aMetacelloVersionSpec
]

{ #category : 'accessing' }
MetacelloMVSBaselineReading >> warning: aKind text: aString [

	^ self add: (MetacelloResolutionFinding warning: aKind text: aString request: request)
]
