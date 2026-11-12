"
I am what a version source answers for one version: the Monticello definitions of its BaselineOf class, as read from wherever the source keeps it, with the problems the source found while reading it. MetacelloMVSGraph runs them in the sandbox.

definitions: the class and method definitions of the BaselineOf, or nil when there is no baseline to read (then problems say why).
repositoryDescription: where the baseline was read from, as repository:version/path, what the baseline sees as its own repository.
problems: { kind. severity. text } for each problem found while reading (no baseline, an obsolete file format, a written path). The graph makes them findings, about the request that first reads this version.
"
Class {
	#name : 'MetacelloMVSBaselineDefinitions',
	#superclass : 'Object',
	#instVars : [
		'definitions',
		'repositoryDescription',
		'problems'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSBaselineDefinitions class >> baselineClassNamed: aClassName method: aMethodSource repository: aString [
	"The definitions of a BaselineOf subclass named aClassName with the single method aMethodSource, as baselines written by hand build them"

	^ self
		  definitions: {
				  ((MCClassDefinition named: aClassName)
					   superclassName: #BaselineOf;
					   category: aClassName;
					   yourself).
				  (MCMethodDefinition
					   className: aClassName
					   selector: (OCParser parseMethod: aMethodSource) selector
					   category: #baselines
					   timeStamp: ''
					   source: aMethodSource) }
		  repository: aString
]

{ #category : 'instance creation' }
MetacelloMVSBaselineDefinitions class >> definitions: aCollection repository: aString [

	^ self new setDefinitions: aCollection repository: aString
]

{ #category : 'instance creation' }
MetacelloMVSBaselineDefinitions class >> error: aKind text: aString [
	"No baseline, because of an error"

	^ self none
		  error: aKind text: aString;
		  yourself
]

{ #category : 'instance creation' }
MetacelloMVSBaselineDefinitions class >> none [
	"No baseline was found: the problems added say why"

	^ self new
]

{ #category : 'accessing' }
MetacelloMVSBaselineDefinitions >> definitions [

	^ definitions
]

{ #category : 'accessing' }
MetacelloMVSBaselineDefinitions >> error: aKind text: aString [
	"Keep the problem as { kind. severity. text }: who asked is known only when the graph reads me"

	^ problems add: {
			  aKind.
			  #error.
			  aString }
]

{ #category : 'accessing' }
MetacelloMVSBaselineDefinitions >> hasBaseline [

	^ definitions notNil
]

{ #category : 'initialization' }
MetacelloMVSBaselineDefinitions >> initialize [

	super initialize.
	problems := OrderedCollection new
]

{ #category : 'accessing' }
MetacelloMVSBaselineDefinitions >> problems [

	^ problems
]

{ #category : 'accessing' }
MetacelloMVSBaselineDefinitions >> repositoryDescription [

	^ repositoryDescription
]

{ #category : 'accessing' }
MetacelloMVSBaselineDefinitions >> setDefinitions: aCollection repository: aString [

	definitions := aCollection.
	repositoryDescription := aString
]

{ #category : 'accessing' }
MetacelloMVSBaselineDefinitions >> warning: aKind text: aString [
	"Keep the problem as { kind. severity. text }: who asked is known only when the graph reads me"

	^ problems add: {
			  aKind.
			  #warning.
			  aString }
]
