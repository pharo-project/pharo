"
I am one version of a project, with what its baseline declares: what it requires, which depends on the groups loaded from it, its replaces, and the findings of reading it. I do not depend on who asks for me: the candidates of that version share me.

baselineClass: the BaselineOf class the sandbox compiled, installed nowhere; newProject builds from it the Metacello project that Metacello records from.
versionSpec: the Metacello version spec of that class. referencesFor:into: expands groups with it, through the same walk Metacello records with (MetacelloVersionSpec>>#packageSpecsInLoadOrderFor:).
repositoryDescription: where the baseline was read from (repository:version/path), where its packages are.
references: what a load of the default groups requires. A project version given by hand (tests) has only these, whatever the groups.
replacements: project name -> repository string, from spec replace:with:.
request: the request of the first candidate that read me; my findings are about it.
findings: what reading the baseline found, the same whatever the groups.
"
Class {
	#name : 'MetacelloMVSProjectVersion',
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
MetacelloMVSProjectVersion class >> references: aCollection [

	^ self new references: aCollection
]

{ #category : 'accessing' }
MetacelloMVSProjectVersion >> add: aFinding [

	findings add: aFinding.
	^ aFinding
]

{ #category : 'groups' }
MetacelloMVSProjectVersion >> baselineClass [
	"The BaselineOf class the sandbox compiled, installed nowhere; nil when the project version was given by hand"

	^ baselineClass
]

{ #category : 'groups' }
MetacelloMVSProjectVersion >> baselineClass: aClass [

	baselineClass := aClass.
	versionSpec := aClass version spec
]

{ #category : 'accessing' }
MetacelloMVSProjectVersion >> error: aKind text: aString [

	^ self add: (MetacelloResolutionFinding error: aKind text: aString request: request)
]

{ #category : 'accessing' }
MetacelloMVSProjectVersion >> findings [

	^ findings
]

{ #category : 'accessing' }
MetacelloMVSProjectVersion >> hasErrors [

	^ findings anySatisfy: [ :each | each isError ]
]

{ #category : 'initialization' }
MetacelloMVSProjectVersion >> initialize [

	super initialize.
	references := #(  ).
	replacements := Dictionary new.
	findings := OrderedCollection new
]

{ #category : 'groups' }
MetacelloMVSProjectVersion >> newProject [
	"A Metacello project of the baseline, whose packages come from where it was read: what Metacello records from"

	^ baselineClass new initalizeProjectWithRepositoryDescription: { repositoryDescription }
]

{ #category : 'accessing' }
MetacelloMVSProjectVersion >> references [

	^ references
]

{ #category : 'accessing' }
MetacelloMVSProjectVersion >> references: aCollection [

	references := aCollection asArray
]

{ #category : 'groups' }
MetacelloMVSProjectVersion >> referencesFor: aCollectionOfNames into: aFindingsCollector [
	"The project specs a load of the groups (or packages) aCollectionOfNames requires, the default ones when empty: what Metacello would record for that load, through includes and requires. A name the baseline does not declare (#missingGroup) is an error given to aFindingsCollector (anything that understands error:text:)"

	versionSpec ifNil: [ ^ references ].
	^ (([ versionSpec packageSpecsInLoadOrderFor: (aCollectionOfNames ifEmpty: [ versionSpec defaultPackageNames ]) ]
		    on: Error
		    do: [ :error |
				    aFindingsCollector error: #missingGroup text: error messageText.
				    ^ #(  ) ]) select: [ :each | each isProjectReferenceSpec ]) collect: [ :each | each projectReference copy ]
]

{ #category : 'groups' }
MetacelloMVSProjectVersion >> referencesForGroups: aCollectionOfNames includingDefault: aBoolean into: aFindingsCollector [
	"The projects a load of the groups aCollectionOfNames, plus the default ones when aBoolean, requires. Project versions given by hand answer their references whatever the groups"

	| names |
	versionSpec ifNil: [ ^ references ].
	names := aCollectionOfNames asOrderedCollection.
	aBoolean ifTrue: [ names addAll: versionSpec defaultPackageNames ].
	^ self referencesFor: names asSet asArray into: aFindingsCollector
]

{ #category : 'accessing' }
MetacelloMVSProjectVersion >> replacements [

	^ replacements
]

{ #category : 'accessing' }
MetacelloMVSProjectVersion >> replacements: aDictionary [

	replacements := aDictionary
]

{ #category : 'groups' }
MetacelloMVSProjectVersion >> repositoryDescription [
	"Where the baseline was read from, as repository:version/path: where its packages are"

	^ repositoryDescription
]

{ #category : 'groups' }
MetacelloMVSProjectVersion >> repositoryDescription: aString [

	repositoryDescription := aString
]

{ #category : 'accessing' }
MetacelloMVSProjectVersion >> request: aProjectRequest [
	"The request of the first candidate that read me: the problems I find are about it"

	request := aProjectRequest
]

{ #category : 'groups' }
MetacelloMVSProjectVersion >> versionSpec [

	^ versionSpec
]

{ #category : 'groups' }
MetacelloMVSProjectVersion >> versionSpec: aMetacelloVersionSpec [

	versionSpec := aMetacelloVersionSpec
]

{ #category : 'accessing' }
MetacelloMVSProjectVersion >> warning: aKind text: aString [

	^ self add: (MetacelloResolutionFinding warning: aKind text: aString request: request)
]
