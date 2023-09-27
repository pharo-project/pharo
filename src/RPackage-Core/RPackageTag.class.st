"
I'm a class tag for an RPackage. 
Class tags can be any kind of metadata the user needs, but they are for now equivalent to class categories. 
"
Class {
	#name : 'RPackageTag',
	#superclass : 'Object',
	#instVars : [
		'package',
		'name',
		'classes'
	],
	#category : 'RPackage-Core-Base',
	#package : 'RPackage-Core',
	#tag : 'Base'
}

{ #category : 'instance creation' }
RPackageTag class >> package: aPackage name: aString [

	^ self new
		  package: aPackage;
		  name: aString;
		  yourself
]

{ #category : 'accessing' }
RPackageTag >> addClass: aClass [
	"I cannot just add the class myself because I need to be sure it is removed from the old packages and that extension methods are managed.
	So I deleate to the package."

	self package moveClass: aClass toTag: self
]

{ #category : 'private' }
RPackageTag >> basicRenameTo: aString [
	name := aString
]

{ #category : 'accessing' }
RPackageTag >> categoryName [
	"Answer this tag as a SystemCategory.
    That would be RPackage, '-', RPackageTag, unless tag is root (same name as RPackage)"
	^ self toCategoryName: self name
]

{ #category : 'accessing' }
RPackageTag >> classNames [

	^ self classes collect: [ :class | class name ]
]

{ #category : 'accessing' }
RPackageTag >> classes [

	^ classes
]

{ #category : 'accessing' }
RPackageTag >> environment [

	^ self package environment
]

{ #category : 'testing' }
RPackageTag >> hasClass: aClass [

	^ self classes includes: aClass instanceSide
]

{ #category : 'testing' }
RPackageTag >> hasClassNamed: aSymbol [
	^ self classNames includes: aSymbol
]

{ #category : 'testing' }
RPackageTag >> includesClass: aClass [
	"To deprecate in favor of #hasClass:"

	^ self hasClass: aClass
]

{ #category : 'initialization' }
RPackageTag >> initialize [

	super initialize.
	classes := IdentitySet new
]

{ #category : 'testing' }
RPackageTag >> isEmpty [
	^ self classNames isEmpty
]

{ #category : 'testing' }
RPackageTag >> isRoot [

	^ self name = self package rootTagName
]

{ #category : 'accessing' }
RPackageTag >> name [
	^ name
]

{ #category : 'accessing' }
RPackageTag >> name: aSymbol [

	name := aSymbol asSymbol
]

{ #category : 'accessing' }
RPackageTag >> organizer [
	^ self package organizer
]

{ #category : 'accessing' }
RPackageTag >> package [
	^ package
]

{ #category : 'accessing' }
RPackageTag >> package: anObject [

	package := anObject
]

{ #category : 'accessing' }
RPackageTag >> packageName [
	^ self package name
]

{ #category : 'printing' }
RPackageTag >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self name.
	aStream nextPut: $)
]

{ #category : 'private' }
RPackageTag >> privateAddClass: aClass [
	"This method adds the class to the tag but does not check is the class was already in the right package."

	classes add: aClass.
	self organizer registerPackage: self package forClass: aClass.

	self flag: #package. "Next line should first use #basicCategory: to not start a ping pong then be totally removed once we will not need the category"
	aClass category: self categoryName
]

{ #category : 'converting' }
RPackageTag >> promoteAsPackage [
	"This method converts this package tag into a package"

	| newPackage |
	self flag: #package. "We need to create and register in two steps because we cannot have a package X-Y and a package X with a tag Y at the same time because of the system organizer. Once the system organizer is not here anymore, we should just do a `self organizer ensurePackage: self package name , '-' , self name` to avoid to have a manual package creation AND a registration."
	newPackage := RPackage named: self categoryName organizer: self organizer.

	self classes do: [ :class | newPackage moveClass: class toTag: newPackage rootTag ].

	self organizer addPackage: newPackage
]

{ #category : 'accessing' }
RPackageTag >> removeClass: aClass [

	| oldObject |
	oldObject := classes remove: aClass ifAbsent: [  ].
	self organizer unregisterPackage: self package forClass: aClass.
	self isEmpty ifTrue: [ self removeFromPackage ].
	^ oldObject
]

{ #category : 'accessing' }
RPackageTag >> removeFromPackage [

	self assert: self isEmpty description: 'Package tag is not empty'.

	self package removeTag: self
]

{ #category : 'accessing' }
RPackageTag >> removeFromSystem [
	"Remove the tag and its content from the system"

	self package removeTag: self
]

{ #category : 'accessing' }
RPackageTag >> renameTo: newTagName [

	| oldCategoryName newCategoryName oldTagName |
	oldTagName := self name.
	oldCategoryName := self toCategoryName: oldTagName.
	newCategoryName := self toCategoryName: newTagName.

	oldTagName = newTagName ifTrue: [ ^ self ].

	self basicRenameTo: newTagName.

	self flag: #package. "This should be removed later when the class will know its tag."
	self classes do: [ :class | class basicCategory: newCategoryName ].
	SystemAnnouncer announce: (PackageTagRenamed to: self oldName: oldTagName newName: newTagName)
]

{ #category : 'accessing' }
RPackageTag >> rootTagName [

	^ self package rootTagName
]

{ #category : 'private' }
RPackageTag >> toCategoryName: aString [
	^ aString = self packageName
		ifTrue: [ aString ]
		ifFalse: [ self packageName, '-', aString ]
]
