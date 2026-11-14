"
I am what a resolver answers: every project a load needs, each at the version chosen (MetacelloResolvedProject), and what the resolver found on the way (MetacelloResolutionFinding). Recording and loading follow me and decide nothing; a load goes ahead only when I have no errors.
roots: the MetacelloResolvedProjects the load asked for, in order.
projects: project class name -> MetacelloResolvedProject, the roots included.
findings: errors and warnings, in the order found.
"
Class {
	#name : 'MetacelloResolution',
	#superclass : 'Object',
	#instVars : [
		'projects',
		'findings',
		'roots'
	],
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'accessing' }
MetacelloResolution >> add: aResolvedProject [

	projects at: aResolvedProject className put: aResolvedProject.
	^ aResolvedProject
]

{ #category : 'findings' }
MetacelloResolution >> addFinding: aFinding [

	findings add: aFinding.
	^ aFinding
]

{ #category : 'accessing' }
MetacelloResolution >> addRoot: aResolvedProject [
	"Add aResolvedProject as a root of the load, after the roots added before.
	roots holds the projects the load asked for; projects holds every project of the resolution: the roots, and the projects they depend on. So a root goes in both"

	roots add: (self add: aResolvedProject)
]

{ #category : 'report' }
MetacelloResolution >> descriptionOfFinding: aFinding [
	"What aFinding says, for people. Resolvers that know more (who required what it is about) say it"

	^ aFinding text
]

{ #category : 'report' }
MetacelloResolution >> descriptionOfKind: aKind [
	"What a kind of finding means, for people. A kind no description names shows as it is"

	^ self kindDescriptions at: aKind ifAbsent: [ aKind asString ]
]

{ #category : 'report' }
MetacelloResolution >> descriptionOfSeverity: aSeverity [
	"A severity for people"

	^ {
		  (#error -> 'Errors').
		  (#warning -> 'Warnings').
		  (#info -> 'Information') } asDictionary at: aSeverity
]

{ #category : 'findings' }
MetacelloResolution >> errors [

	^ findings select: [ :each | each isError ]
]

{ #category : 'findings' }
MetacelloResolution >> findings [

	^ findings
]

{ #category : 'findings' }
MetacelloResolution >> hasErrors [

	^ findings anySatisfy: [ :each | each isError ]
]

{ #category : 'accessing' }
MetacelloResolution >> includes: aProjectSpec [

	^ projects includesKey: aProjectSpec className
]

{ #category : 'findings' }
MetacelloResolution >> infos [

	^ findings select: [ :each | each isInfo ]
]

{ #category : 'initialization' }
MetacelloResolution >> initialize [

	super initialize.
	projects := Dictionary new.
	roots := OrderedCollection new.
	findings := OrderedCollection new
]

{ #category : 'report' }
MetacelloResolution >> kindDescriptions [
	"Kind -> what it means, for people. The default resolver finds nothing; resolvers that do describe their kinds"

	^ Dictionary new
]

{ #category : 'report' }
MetacelloResolution >> print: aString asFindingOn: aStream [
	"One finding of the report, each line indented under its kind"

	| lines |
	lines := aString lines.
	aStream
		lf;
		nextPutAll: '  - ';
		nextPutAll: lines first.
	lines allButFirst do: [ :each |
			aStream
				lf;
				nextPutAll: '    ';
				nextPutAll: each ]
]

{ #category : 'report' }
MetacelloResolution >> print: someFindings of: aSeverity verbose: aBoolean on: aStream [
	"The findings of aSeverity, counted per kind. Verbose, each is listed with who required what it is about; otherwise only errors are listed, one line each"

	((someFindings groupedBy: [ :each | self descriptionOfKind: each kind ]) associations sort: [ :a :b | a key <= b key ]) do: [
			:kind |
			aStream
				lf;
				nextPutAll: kind key;
				nextPutAll: ' (';
				print: kind value size;
				nextPut: $).
			aBoolean
				ifTrue: [ kind value do: [ :each | self print: (self descriptionOfFinding: each) asFindingOn: aStream ] ]
				ifFalse: [
				aSeverity = #error ifTrue: [ kind value do: [ :each | self print: (self shortDescriptionOf: each) asFindingOn: aStream ] ] ] ]
]

{ #category : 'report' }
MetacelloResolution >> printReportOn: aStream [

	self printReportOn: aStream verbose: false
]

{ #category : 'report' }
MetacelloResolution >> printReportOn: aStream verbose: aBoolean [
	"How many findings of each severity, then each severity in turn under a title between bars (see print:of:verbose:on:). The short report says where the details are"

	aStream
		print: self errors size;
		nextPutAll: ' errors, ';
		print: self warnings size;
		nextPutAll: ' warnings, ';
		print: self infos size;
		nextPutAll: ' infos'.
	aBoolean ifFalse: [
			aStream
				lf;
				nextPutAll: 'Check verbose report for issue details' ].
	#( #error #warning #info ) do: [ :severity |
			| ofSeverity |
			ofSeverity := findings select: [ :each | each severity = severity ].
			ofSeverity ifNotEmpty: [
					self printTitle: (self descriptionOfSeverity: severity) , ' (' , ofSeverity size printString , ')' on: aStream.
					self
						print: ofSeverity
						of: severity
						verbose: aBoolean
						on: aStream ] ]
]

{ #category : 'report' }
MetacelloResolution >> printTitle: aString on: aStream [
	"A title between two bars as long as it, after a blank line"

	| bar |
	bar := String new: aString size withAll: $-.
	aStream
		lf;
		lf;
		nextPutAll: bar;
		lf;
		nextPutAll: aString;
		lf;
		nextPutAll: bar
]

{ #category : 'accessing' }
MetacelloResolution >> projectFor: aProjectSpec ifAbsent: aBlock [
	"The resolved project aProjectSpec (a root or a reference, at any version) names"

	^ projects at: aProjectSpec className ifAbsent: aBlock
]

{ #category : 'accessing' }
MetacelloResolution >> projects [

	^ projects values
]

{ #category : 'report' }
MetacelloResolution >> report [
	"What I found, as text for people, warnings only counted (see printReportOn:verbose:)"

	^ String streamContents: [ :stream | self printReportOn: stream verbose: false ]
]

{ #category : 'accessing' }
MetacelloResolution >> roots [
	"The projects the load asked for, each loaded with the groups asked of it: one, unless the load names several roots"

	^ roots
]

{ #category : 'report' }
MetacelloResolution >> shortDescriptionOf: aFinding [
	"What aFinding says, on one line, for the short report. Resolvers that know where it is say it"

	^ aFinding text lines first
]

{ #category : 'report' }
MetacelloResolution >> verboseReport [
	"What I found, as text for people, warnings listed one by one with who required what they are about (see printReportOn:verbose:)"

	^ String streamContents: [ :stream | self printReportOn: stream verbose: true ]
]

{ #category : 'findings' }
MetacelloResolution >> warnings [

	^ findings select: [ :each | each isWarning ]
]
