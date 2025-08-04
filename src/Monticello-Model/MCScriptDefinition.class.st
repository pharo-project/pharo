"
Common superclass for script definitions
"
Class {
	#name : 'MCScriptDefinition',
	#superclass : 'MCDefinition',
	#instVars : [
		'script',
		'packageName'
	],
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'instance creation' }
MCScriptDefinition class >> from: aPackageInfo [
	^ self script: (aPackageInfo perform: self scriptSelector) contents asString packageName: aPackageInfo name
]

{ #category : 'instance creation' }
MCScriptDefinition class >> script: aString packageName: packageString [
	^ self new initializeWithScript: aString packageName: packageString
]

{ #category : 'accessing' }
MCScriptDefinition class >> scriptSelector [
	self subclassResponsibility
]

{ #category : 'instance creation' }
MCScriptDefinition class >> scriptSelector: selectorString script: aString packageName: packageString [
	^ (self subclassForScriptSelector: selectorString)
		script: aString packageName: packageString
]

{ #category : 'accessing' }
MCScriptDefinition class >> subclassForScriptSelector: selectorString [
	^self allSubclasses detect: [:ea | ea scriptSelector = selectorString]
]

{ #category : 'comparing' }
MCScriptDefinition >> = aDefinition [
	^ (super = aDefinition)
		and: [script = aDefinition script]
]

{ #category : 'visiting' }
MCScriptDefinition >> accept: aVisitor [
	aVisitor visitScriptDefinition: self
]

{ #category : 'accessing' }
MCScriptDefinition >> description [
	^ Array with: packageName with: self scriptSelector
]

{ #category : 'installing' }
MCScriptDefinition >> evaluate [
	self class compiler evaluate: script
]

{ #category : 'initialization' }
MCScriptDefinition >> initializeWithScript: aString packageName: packageString [
	script := aString.
	packageName := packageString
]

{ #category : 'installing' }
MCScriptDefinition >> installScript [
	self installScript: script
]

{ #category : 'installing' }
MCScriptDefinition >> installScript: aString [

	self systemPackage ifNotNil: [ :package |
		| selector |
		selector := self scriptSelector asMutator.
		(package respondsTo: selector) ifTrue: [ package perform: selector with: aString ] ]
]

{ #category : 'testing' }
MCScriptDefinition >> isScriptDefinition [
	^true
]

{ #category : 'installing' }
MCScriptDefinition >> load [
	self installScript
]

{ #category : 'accessing' }
MCScriptDefinition >> packageName [
	^ packageName
]

{ #category : 'accessing' }
MCScriptDefinition >> script [
	^ script
]

{ #category : 'accessing' }
MCScriptDefinition >> scriptSelector [
	^ self class scriptSelector
]

{ #category : 'accessing' }
MCScriptDefinition >> sortKey [
	^ '!', self scriptSelector "force to the front so it gets loaded first"
]

{ #category : 'accessing' }
MCScriptDefinition >> source [
	^ script
]

{ #category : 'accessing' }
MCScriptDefinition >> summary [
	^ packageName, ' ', self scriptSelector
]

{ #category : 'accessing' }
MCScriptDefinition >> systemPackage [
	"Return the corresponding system package. Return nil if no package in the system has this name."

	^ self class environment organization packageNamed: self packageName ifAbsent: [ nil ]
]

{ #category : 'installing' }
MCScriptDefinition >> unload [
	self installScript: nil
]
