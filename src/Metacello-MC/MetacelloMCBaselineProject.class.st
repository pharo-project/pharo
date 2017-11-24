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
	#name : #MetacelloMCBaselineProject,
	#superclass : #MetacelloMCProject,
	#category : #'Metacello-MC-Model'
}

{ #category : #accessing }
MetacelloMCBaselineProject class >> singletonVersionName [
    ^ 'baseline'
]

{ #category : #accessing }
MetacelloMCBaselineProject class >> versionConstructorClass [
    ^ MetacelloBaselineConstructor
]

{ #category : #scripting }
MetacelloMCBaselineProject >> asBaselineProject [
    ^ self
]

{ #category : #scripting }
MetacelloMCBaselineProject >> asConfigurationProject [
    ^ MetacelloMCProject new
]

{ #category : #versions }
MetacelloMCBaselineProject >> bleedingEdge [
    ^ nil
]

{ #category : #versions }
MetacelloMCBaselineProject >> currentVersion [
    ^ self version isSomethingLoaded
        ifTrue: [ self version ]
        ifFalse: [ nil ]
]

{ #category : #versions }
MetacelloMCBaselineProject >> currentVersionAgainst: resolvedPackageAndProjectNames [
    ^ nil
]

{ #category : #versions }
MetacelloMCBaselineProject >> development [
    ^ nil
]

{ #category : #versions }
MetacelloMCBaselineProject >> hasVersion: versionString [
    ^ versionString = 'baseline'
]

{ #category : #versions }
MetacelloMCBaselineProject >> lastVersion [
    ^ self version
]

{ #category : #versions }
MetacelloMCBaselineProject >> latestVersion [
    ^ self version
]

{ #category : #versions }
MetacelloMCBaselineProject >> latestVersion: blessing [
    ^ nil
]

{ #category : #versions }
MetacelloMCBaselineProject >> latestVersionMatching: versionPatternString [
    ^ nil
]

{ #category : #versions }
MetacelloMCBaselineProject >> latestVersionMatching: versionPatternString excludedBlessings: excluded [
    ^ nil
]

{ #category : #versions }
MetacelloMCBaselineProject >> latestVersionMatching: versionPatternString includedBlessings: included [
    ^ nil
]

{ #category : #versions }
MetacelloMCBaselineProject >> latestVersionMatching: versionPatternString includedBlessings: included excludedBlessings: excludedBlessings [
    ^ nil
]

{ #category : #scripting }
MetacelloMCBaselineProject >> projectForScriptEngine: aMetacelloScriptEngine unconditionalLoad: aBool [
  "see https://github.com/dalehenrich/metacello-work/issues/244 ... uncoditionally load
   baseline"

  ^ aMetacelloScriptEngine getBaselineProjectUnconditionalLoad: true
]

{ #category : #'as yet unclassified' }
MetacelloMCBaselineProject >> setBaselineRepositoryDescription: aListOrRepositoryDescriptions [
    "set #version repositories to < aListOrRepositoryDescriptions>. Should be the directory where the BaselineOf is located."

    aListOrRepositoryDescriptions do: [:desc | self version spec repository: desc]
]

{ #category : #versions }
MetacelloMCBaselineProject >> singletonVersionName [
    ^ self class singletonVersionName
]

{ #category : #versions }
MetacelloMCBaselineProject >> stableVersion [
    ^ nil
]

{ #category : #versions }
MetacelloMCBaselineProject >> symbolicVersionSymbols [
    ^ nil
]

{ #category : #versions }
MetacelloMCBaselineProject >> version [
    ^ self version: self singletonVersionName
]

{ #category : #versions }
MetacelloMCBaselineProject >> versions [
  ^ [ {(self version)} ]
    on: MetacelloVersionDoesNotExistError
    do: [ :ex | ^ #() ]
]
