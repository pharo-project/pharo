"
I'm a class tag for an RPackage. 
Class tags can be any kind of metadata the user needs, but they are for now equivalent to class categories. 
"
Class {
	#name : #RPackageTag,
	#superclass : #Object,
	#instVars : [
		'package',
		'name',
		'classes'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #'instance creation' }
RPackageTag class >> package: aPackage name: aString [

	^ self new
		  package: aPackage;
		  name: aString;
		  yourself
]

{ #category : #accessing }
RPackageTag >> addClass: aClass [

	aClass category = self categoryName ifFalse: [ aClass category: self categoryName ].
	classes add: aClass
]

{ #category : #private }
RPackageTag >> basicRenameTo: aString [
	name := aString
]

{ #category : #accessing }
RPackageTag >> categoryName [
	"Answer this tag as a SystemCategory.
    That would be RPackage, '-', RPackageTag, unless tag is root (same name as RPackage)"
	^ self toCategoryName: self name
]

{ #category : #accessing }
RPackageTag >> classNames [

	^ self classes collect: [ :class | class name ]
]

{ #category : #accessing }
RPackageTag >> classes [

	^ classes
]

{ #category : #accessing }
RPackageTag >> environment [

	^ self package environment
]

{ #category : #testing }
RPackageTag >> hasClass: aClass [

	^ self classes includes: aClass instanceSide
]

{ #category : #testing }
RPackageTag >> hasClassNamed: aSymbol [
	^ self classNames includes: aSymbol
]

{ #category : #testing }
RPackageTag >> includesClass: aClass [
	^ self hasClassNamed: aClass name
]

{ #category : #initialization }
RPackageTag >> initialize [

	super initialize.
	classes := IdentitySet new
]

{ #category : #testing }
RPackageTag >> isEmpty [
	^ self classNames isEmpty
]

{ #category : #testing }
RPackageTag >> isRoot [
	^ self name = self packageName
]

{ #category : #accessing }
RPackageTag >> name [
	^ name
]

{ #category : #accessing }
RPackageTag >> name: aSymbol [

	name := aSymbol asSymbol
]

{ #category : #accessing }
RPackageTag >> organizer [
	^ self package organizer
]

{ #category : #accessing }
RPackageTag >> package [
	^ package
]

{ #category : #accessing }
RPackageTag >> package: anObject [

	package := anObject
]

{ #category : #accessing }
RPackageTag >> packageName [
	^ self package name
]

{ #category : #printing }
RPackageTag >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self name.
	aStream nextPut: $)
]

{ #category : #converting }
RPackageTag >> promoteAsPackage [
	"This method converts this package tag into a package"

	| newPackage |
	self flag: #package. "We need to create and register in two steps because we cannot have a package X-Y and a package X with a tag Y at the same time because of the system organizer. Once the system organizer is not here anymore, we should just do a `self organizer ensurePackage: self package name , '-' , self name` to avoid to have a manual package creation AND a registration."
	newPackage := RPackage named: self categoryName organizer: self organizer.

	self classes do: [ :class | newPackage moveClass: class toTag: newPackage rootTag ].

	self organizer registerPackage: newPackage
]

{ #category : #accessing }
RPackageTag >> removeClass: aClass [

	^ classes remove: aClass ifAbsent: [  ]
]

{ #category : #accessing }
RPackageTag >> removeFromPackage [

	self assert: self isEmpty description: 'Package tag is not empty'.

	self package removeTag: self
]

{ #category : #accessing }
RPackageTag >> renameTo: newTagName [

	| oldName categoryName tagName |
	tagName := self name.
	oldName := self toCategoryName: tagName.
	categoryName := self toCategoryName: newTagName.

	oldName = categoryName ifTrue: [ ^ self ].

	self basicRenameTo: newTagName.
	self environmentAnnouncer suspendAllWhile: [
		self classes do: [ :each | each category: categoryName ].
		self organizer renameCategory: oldName toBe: categoryName ].
	self environmentAnnouncer announce: (PackageTagRenamed to: self oldName: tagName newName: newTagName)
]

{ #category : #accessing }
RPackageTag >> renameTo: aString category: categoryName [

	| oldName |
	oldName := self toCategoryName: self name.
	oldName = categoryName ifTrue: [ ^ self ].

	self basicRenameTo: aString.
	self environmentAnnouncer suspendAllWhile: [
		self classes do: [ :each | each category: categoryName ].
		self organizer renameCategory: oldName toBe: categoryName ]
]

{ #category : #accessing }
RPackageTag >> rootTagName [

	^ self package rootTagName
]

{ #category : #private }
RPackageTag >> toCategoryName: aString [
	^ aString = self packageName
		ifTrue: [ aString ]
		ifFalse: [ self packageName, '-', aString ]
]
