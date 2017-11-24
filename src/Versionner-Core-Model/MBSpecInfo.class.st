"
A MBSpecInfo holds information on Metacello specifications.

Instance Variables
	configurationInfo:		the object holding information on the whole configuration.
	spec:		the specification of this object (a MetacelloSpec).

"
Class {
	#name : #MBSpecInfo,
	#superclass : #MBInfo,
	#instVars : [
		'spec',
		'configurationInfo'
	],
	#category : #'Versionner-Core-Model'
}

{ #category : #'instance creation' }
MBSpecInfo class >> spec: aMetacelloSpec [
	"Create an instance of myself with the given spec"

	^(super new) spec: aMetacelloSpec
]

{ #category : #commands }
MBSpecInfo >> cmdLoadSpecCommand [
	<command>
	^ MBLoadPackageCommand
]

{ #category : #accessing }
MBSpecInfo >> configurationInfo [
	^ configurationInfo
]

{ #category : #accessing }
MBSpecInfo >> configurationInfo: anObject [
	configurationInfo := anObject
]

{ #category : #testing }
MBSpecInfo >> currentVersionMismatch [

	^false
]

{ #category : #testing }
MBSpecInfo >> isDirty [
	| wc |
	wc := self workingCopy.
	wc ifNil: [ ^false ].
	^wc needsSaving
]

{ #category : #accessing }
MBSpecInfo >> packageName [

	^self name
]

{ #category : #accessing }
MBSpecInfo >> project [
	"Answer the MetacelloProject associated with the receiver."
	
	^self spec project
]

{ #category : #accessing }
MBSpecInfo >> spec [

	^spec
]

{ #category : #accessing }
MBSpecInfo >> spec: aMetacelloSpec [

	spec := aMetacelloSpec 
]

{ #category : #accessing }
MBSpecInfo >> typeLabel [

	^''
]

{ #category : #accessing }
MBSpecInfo >> validate [

	^nil
]

{ #category : #accessing }
MBSpecInfo >> validationIcon [

	^nil
]

{ #category : #accessing }
MBSpecInfo >> validationIconSymbol [

	^#blank
]
