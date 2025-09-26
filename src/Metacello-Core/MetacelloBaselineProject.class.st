"
The **MetacelloMCBaselineProject**  is a wrapper for the **BaselineOf** version specification for file-based repositories.

There is a single version in a **MetacelloMCBaselineProject**, named *'baseline'*.

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
	#name : 'MetacelloBaselineProject',
	#superclass : 'MetacelloProject',
	#instVars : [
		'repositoryDescription'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'accessing' }
MetacelloBaselineProject class >> singletonVersionName [
    ^ 'baseline'
]

{ #category : 'scripting' }
MetacelloBaselineProject >> asBaselineProject [
    ^ self
]

{ #category : 'scripting' }
MetacelloBaselineProject >> asConfigurationProject [
    ^ MetacelloProject new
]

{ #category : 'versions' }
MetacelloBaselineProject >> bleedingEdge [
    ^ nil
]

{ #category : 'versions' }
MetacelloBaselineProject >> currentVersionAgainst: resolvedPackageAndProjectNames [
    ^ nil
]

{ #category : 'versions' }
MetacelloBaselineProject >> development [
    ^ nil
]

{ #category : 'versions' }
MetacelloBaselineProject >> hasVersion: versionString [
    ^ versionString = 'baseline'
]

{ #category : 'versions' }
MetacelloBaselineProject >> lastVersion [
    ^ self version
]

{ #category : 'versions' }
MetacelloBaselineProject >> latestVersion [
    ^ self version
]

{ #category : 'versions' }
MetacelloBaselineProject >> latestVersion: blessing [
    ^ nil
]

{ #category : 'accessing' }
MetacelloBaselineProject >> repositoryDescription [
	^ repositoryDescription
]

{ #category : 'development support' }
MetacelloBaselineProject >> setBaselineRepositoryDescription: aListOrRepositoryDescriptions [
	"set #version repositories to < aListOrRepositoryDescriptions>. Should be the directory where the BaselineOf is located."

	repositoryDescription := aListOrRepositoryDescriptions
]

{ #category : 'versions' }
MetacelloBaselineProject >> singletonVersionName [
    ^ self class singletonVersionName
]

{ #category : 'versions' }
MetacelloBaselineProject >> stableVersion [
    ^ nil
]

{ #category : 'versions' }
MetacelloBaselineProject >> symbolicVersionSymbols [
    ^ nil
]

{ #category : 'versions' }
MetacelloBaselineProject >> version [
    ^ self version: self singletonVersionName
]

{ #category : 'versions' }
MetacelloBaselineProject >> versions [
  ^ [ {(self version)} ]
    on: MetacelloVersionDoesNotExistError
    do: [ :ex | ^ #() ]
]
