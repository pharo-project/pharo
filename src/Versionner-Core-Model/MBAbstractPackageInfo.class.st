"
A MBAbstractPackageInfo is an abstraction to represent both project and packages.

Instance Variables
	packageName:		name of the package
	workingCopy:		a MCWorkingCopy representing the version of the package in memory

"
Class {
	#name : #MBAbstractPackageInfo,
	#superclass : #MBSpecInfo,
	#instVars : [
		'packageName',
		'workingCopy'
	],
	#category : #Versionner-Core-Model
}

{ #category : #'instance creation' }
MBAbstractPackageInfo class >> named: infoName [

	^self named: infoName packageName: infoName spec: nil
]

{ #category : #'instance creation' }
MBAbstractPackageInfo class >> named: infoName packageName: packageName spec: aMetacelloSpec [
	"Create an instance of myself named infoName for the package named packageName"

	| answer |
	self assert: [ packageName isString ].
	self assert: [ packageName isSymbol not ].
	
	answer := super new.
	answer name: infoName.
	answer packageName: packageName.
	answer spec: aMetacelloSpec.
	^ answer
]

{ #category : #'instance creation' }
MBAbstractPackageInfo class >> new [
	self error: 'Please, use #named: to create an instance'
]

{ #category : #accessing }
MBAbstractPackageInfo >> classForBrowsing [
	self subclassResponsibility 
]

{ #category : #commands }
MBAbstractPackageInfo >> cmdBrowse [
	<command>
	^ MBBrowsePackageCommand
]

{ #category : #commands }
MBAbstractPackageInfo >> cmdBrowsePackageChanges [
	<command>
	^ MBPackageChangesCommand
	
]

{ #category : #commands }
MBAbstractPackageInfo >> cmdOpenRepositoryCommand [
	<command>
	^ VSOpenRepositoryCommand
]

{ #category : #testing }
MBAbstractPackageInfo >> isDirty [
	| wc |
	wc := self workingCopy.
	wc ifNil: [ ^false ].
	^wc needsSaving
]

{ #category : #accessing }
MBAbstractPackageInfo >> packageName [

	^packageName
]

{ #category : #accessing }
MBAbstractPackageInfo >> packageName: aString [

	packageName := aString
]

{ #category : #accessing }
MBAbstractPackageInfo >> workingCopy [
	| pkg |
	workingCopy ~~ nil ifTrue: [ ^workingCopy ].
	pkg := MCPackage named: self packageName.
	pkg  hasWorkingCopy ifFalse: [ ^nil ].
	^workingCopy := pkg workingCopy
]
