"
The **MetacelloProject**  is a wrapper for the **BaselineOf** version specification for file-based repositories.

There is a single version in a **MetacelloProject**, named *'baseline'*.

A typical **BaselineOf** is specification:

```Smalltalk
baseline: spec
    <baseline>
    spec
        package: 'External-Core';
        package: 'External-Tests' with: [ spec requires: 'External-Core' ];
        yourself.
    spec
        group: 'Core' with: #('External-Core');
        group: 'default' with: #('Core');
        group: 'Tests' with: #('External-Tests');
        yourself
```

The `<baseline>` pragma marks the method containing the baseline specification.
"
Class {
	#name : 'MetacelloProject',
	#superclass : 'Object',
	#instVars : [
		'repositoryDescription',
		'loadType',
		'projectAttributes',
		'version',
		'baseline'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'class initialization' }
MetacelloProject class >> initialize [
	"Deprecation added during the development of Pharo 14."

	self deprecatedAliases: #( #MetacelloMCBaselineProject #MetacelloBaselineProject #MetacelloCypressBaselineProject )
]

{ #category : 'private' }
MetacelloProject >> attributes [

	^ (OrderedCollection with: #common)
		  addAll: self platformAttributes;
		  yourself
]

{ #category : 'accessing' }
MetacelloProject >> baseline [

	^ baseline
]

{ #category : 'accessing' }
MetacelloProject >> baseline: aBaseline [

	baseline := aBaseline
]

{ #category : 'private' }
MetacelloProject >> defaultPlatformAttributes [
	"Returns the tags for the conditional platform loading in Metacello. Pay attention the order is important: from most  to least general."

	^ {
		  #squeakCommon.
		  #pharo.
		  ('pharo' , SystemVersion current major asString , '.x') asSymbol.
		  ('pharo' , SystemVersion current major asString , '.0.x') asSymbol }
]

{ #category : 'spec classes' }
MetacelloProject >> groupSpec [

	^ MetacelloGroupSpec project: self
]

{ #category : 'accessing' }
MetacelloProject >> label [

	^ self baseline class name
]

{ #category : 'accessing' }
MetacelloProject >> loadType [
	"#atomic or #linear"

	^ loadType
]

{ #category : 'accessing' }
MetacelloProject >> loadType: aSymbol [
	"#atomic or #linear"

	loadType := aSymbol
]

{ #category : 'spec classes' }
MetacelloProject >> packageSpec [

	^ MetacelloPackageSpec project: self
]

{ #category : 'private' }
MetacelloProject >> platformAttributes [

	| list |
	list := OrderedCollection new.
	self projectAttributes ifNotNil: [ :aBlock | list addAll: aBlock value ].
	^ self defaultPlatformAttributes , list
]

{ #category : 'printing' }
MetacelloProject >> printOn: aStream [

	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: (version
			 ifNil: [ 'No version yet' ]
			 ifNotNil: [ self label ]).
	aStream nextPut: $)
]

{ #category : 'accessing' }
MetacelloProject >> project [

	^self
]

{ #category : 'private' }
MetacelloProject >> projectAttributes [

	^ projectAttributes ifNil: [ #(  ) ]
]

{ #category : 'private' }
MetacelloProject >> projectAttributes: aList [

	projectAttributes := aList
]

{ #category : 'accessing' }
MetacelloProject >> repositoryDescription [
	^ repositoryDescription
]

{ #category : 'development support' }
MetacelloProject >> setBaselineRepositoryDescription: aListOrRepositoryDescriptions [
	"set #version repositories to < aListOrRepositoryDescriptions>. Should be the directory where the BaselineOf is located."

	repositoryDescription := aListOrRepositoryDescriptions
]

{ #category : 'accessing' }
MetacelloProject >> strict [
	
	^ version strict
]

{ #category : 'versions' }
MetacelloProject >> version [

	^ version
]

{ #category : 'versions' }
MetacelloProject >> version: aVersion [

	version := aVersion
]
