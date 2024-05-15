"
I'm a class tag for an RPackage. 
Class tags can be any kind of metadata the user needs, but they are for now equivalent to class categories. 
"
Class {
	#name : 'PackageTag',
	#superclass : 'Object',
	#instVars : [
		'package',
		'name',
		'classes'
	],
	#category : 'Kernel-CodeModel-Packages',
	#package : 'Kernel-CodeModel',
	#tag : 'Packages'
}

{ #category : 'instance creation' }
PackageTag class >> package: aPackage name: aString [

	^ self new
		  package: aPackage;
		  name: aString;
		  yourself
]

{ #category : 'accessing' }
PackageTag >> addClass: aClass [
	"I cannot just add the class myself because I need to be sure it is removed from the old packages and that extension methods are managed.
	So I deleate to the package."

	self package moveClass: aClass toTag: self
]

{ #category : 'private' }
PackageTag >> basicRenameTo: aString [
	name := aString
]

{ #category : 'accessing' }
PackageTag >> categoryName [
	"Answer this tag as a SystemCategory. This should be use the less possible ideally.
    That would be Package, '-', PackageTag, unless tag is root (same name as Package)"

	^ self isRoot
		  ifTrue: [ self packageName ]
		  ifFalse: [ self packageName , '-' , self name ]
]

{ #category : 'accessing' }
PackageTag >> classNames [

	^ self classes collect: [ :class | class name ]
]

{ #category : 'accessing' }
PackageTag >> classes [

	^ classes
]

{ #category : 'accessing' }
PackageTag >> codeChangeAnnouncer [

	^ self environment codeChangeAnnouncer
]

{ #category : 'accessing' }
PackageTag >> environment [

	^ self package environment
]

{ #category : 'testing' }
PackageTag >> hasClassNamed: aSymbol [
	^ self classNames includes: aSymbol
]

{ #category : 'testing' }
PackageTag >> includesClass: aClass [
	"To deprecate in favor of #hasClass:"

	^ self classes includes: aClass instanceSide
]

{ #category : 'initialization' }
PackageTag >> initialize [

	super initialize.
	classes := IdentitySet new
]

{ #category : 'testing' }
PackageTag >> isEmpty [
	^ self classNames isEmpty
]

{ #category : 'testing' }
PackageTag >> isRoot [

	^ self name = self package rootTagName
]

{ #category : 'testing' }
PackageTag >> isUndefined [

	^ false
]

{ #category : 'accessing' }
PackageTag >> name [
	^ name
]

{ #category : 'accessing' }
PackageTag >> name: aSymbol [

	name := aSymbol asSymbol
]

{ #category : 'accessing' }
PackageTag >> organizer [
	^ self package organizer
]

{ #category : 'accessing' }
PackageTag >> package [
	^ package
]

{ #category : 'accessing' }
PackageTag >> package: anObject [

	package := anObject
]

{ #category : 'accessing' }
PackageTag >> packageName [
	^ self package name
]

{ #category : 'printing' }
PackageTag >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self name.
	aStream nextPut: $)
]

{ #category : 'private' }
PackageTag >> privateAddClass: aClass [
	"This method adds the class to the tag but does not check is the class was already in the right package."

	classes add: aClass.
	aClass basicTag: self
]

{ #category : 'converting' }
PackageTag >> promoteAsPackage [
	"This method converts this package tag into a package"

	| newPackage |
	newPackage := self organizer ensurePackage: self package name , '-' , self name.

	self classes do: [ :class | newPackage moveClass: class toTag: newPackage rootTag ]
]

{ #category : 'accessing' }
PackageTag >> removeClass: aClass [

	classes remove: aClass ifAbsent: [ ^ self ].
	self isEmpty ifTrue: [ self removeFromPackage ]
]

{ #category : 'accessing' }
PackageTag >> removeFromPackage [

	self assert: self isEmpty description: 'Package tag is not empty'.

	self package removeTag: self
]

{ #category : 'accessing' }
PackageTag >> removeFromSystem [
	"Remove the tag and its content from the system"

	self package removeTag: self
]

{ #category : 'accessing' }
PackageTag >> renameTo: newTagName [

	| oldTagName |
	oldTagName := self name.

	oldTagName = newTagName ifTrue: [ ^ self ].

	self basicRenameTo: newTagName.

	self codeChangeAnnouncer  announce: (PackageTagRenamed to: self oldName: oldTagName newName: newTagName)
]

{ #category : 'accessing' }
PackageTag >> rootTagName [

	^ self package rootTagName
]
