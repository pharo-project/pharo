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
		'classNames'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #'instance creation' }
RPackageTag class >> new [ 
	self error: 'Use #package:name:'
]

{ #category : #'instance creation' }
RPackageTag class >> package: aPackage name: aString [ 
	^ self basicNew 
		initializePackage: aPackage name: aString;
		yourself
]

{ #category : #accessing }
RPackageTag >> addClass: aClass [
	aClass category: self categoryName
]

{ #category : #accessing }
RPackageTag >> addClassNamed: aSymbol [ 
	(classNames includes: aSymbol) ifTrue: [ ^ self ].
	^ classNames add: aSymbol.
]

{ #category : #'private ' }
RPackageTag >> asRPackage [
	"Create a new RPackage with the same contents as this rpackage tag"
	| newRPackage |
	
	newRPackage := RPackage named: self categoryName.
	self classes 
		do: [ :className | newRPackage importClass: className ].
	(self package extensionsForTag: self) 
		do: [ :extensionMethod | newRPackage addMethod: extensionMethod ].
	
	^ newRPackage
]

{ #category : #'private ' }
RPackageTag >> basicRenameTo: aString [
	name := aString.
]

{ #category : #accessing }
RPackageTag >> categoryName [ 
	"Answer this tag as a SystemCategory. 
    That would be RPackage, '-', RPackageTag, unless tag is root (same name as RPackage)"
	^ self toCategoryName: self name

]

{ #category : #accessing }
RPackageTag >> classNames [ 
	^ classNames
]

{ #category : #accessing }
RPackageTag >> classes [
	^ self classNames collect: [ :each | self class environment at: each ]
]

{ #category : #accessing }
RPackageTag >> ensureSystemCategory [ 
	SystemOrganization addCategory: self categoryName
]

{ #category : #accessing }
RPackageTag >> extendedClasses [
	^ #()
]

{ #category : #accessing }
RPackageTag >> extensionCategoriesForClass: aClass [ 
	^ self package extensionCategoriesForClass: aClass
]

{ #category : #testing }
RPackageTag >> hasClass: aClass [ 
	^  self hasClassNamed: aClass name
]

{ #category : #testing }
RPackageTag >> hasClassNamed: aSymbol [ 
	^ self classNames includes: aSymbol
]

{ #category : #testing }
RPackageTag >> includesClass: aClass [ 
	^ self hasClassNamed: aClass name
]

{ #category : #testing }
RPackageTag >> includesMethodCategory: aProtocol ofClass: aClass [ 
	^ self package 
		includesMethodCategory: aProtocol 
		ofClass: aClass 
]

{ #category : #testing }
RPackageTag >> includesSelector: aSelector ofClass: aClass [
	^ self package 
		includesSelector: aSelector 
		ofClass: aClass
]

{ #category : #initialization }
RPackageTag >> initialize [ 
	super initialize.
	classNames := Set new.
]

{ #category : #initialization }
RPackageTag >> initializePackage: aPackage name: aString [ 
	package := aPackage.
	name := aString.
	self initialize.
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
RPackageTag >> orderedClasses [
^ self package orderedClasses select:[:c | c category = self categoryName]
]

{ #category : #accessing }
RPackageTag >> package [ 
	^ package
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
RPackageTag >> promoteAsRPackage [
	"This method converts this rpackage tag into an rpackage, removes the tag from the parent package with all classes included and registers the new package in the system.
	The tag has to be removed before registering to avoid conflicts.
	Smells like we could have an error and lose package tags! registerPackage should not fail because names Package-Tag are unique in the system."
	| newRPackage |
	
	newRPackage := self asRPackage.
	self classes do: [ :each | self package removeClass: each  ].
	self package removeClassTag: self name.
	self package removeMethods: newRPackage extensionMethods.
	RPackage organizer registerPackage: newRPackage.
	
	newRPackage classes do: [ :each | 
		SystemAnnouncer uniqueInstance 
			classRepackaged: each 
			from: self package
			to:  newRPackage ].
]

{ #category : #accessing }
RPackageTag >> removeClass: aClass [
	^ self removeClassNamed: aClass name.
]

{ #category : #accessing }
RPackageTag >> removeClassNamed: aSymbol [ 
	^ classNames remove: aSymbol ifAbsent: [].
]

{ #category : #accessing }
RPackageTag >> removeFromPackage [
	self 
		assert: self isEmpty 
		description: 'Package tag is not empty'.
		
	self package basicRemoveTag: self
]

{ #category : #accessing }
RPackageTag >> renameTo: newTagName [
	| oldName categoryName tagName |
	tagName := self name.
	oldName := self toCategoryName: tagName.
	categoryName := self toCategoryName: newTagName. 
	
	oldName = categoryName ifTrue: [ ^ self ].
	
	self basicRenameTo: newTagName.
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self classes do: [ :each | each category: categoryName ].
		self class environment organization 
			renameCategory: oldName 
			toBe: categoryName].
	SystemAnnouncer uniqueInstance 
		classTagRenamed: tagName 
		inPackage: self package 
		to: newTagName
]

{ #category : #accessing }
RPackageTag >> renameTo: aString category: categoryName [
	| oldName |
	
	oldName := self toCategoryName: self name.
	oldName = categoryName ifTrue: [ ^ self ].
	
	self basicRenameTo: aString.
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self classes do: [ :each | each category: categoryName ].
		self class environment organization 
			renameCategory: oldName 
			toBe: categoryName. ].
]

{ #category : #'private ' }
RPackageTag >> toCategoryName: aString [ 
	^ aString = self packageName
		ifTrue: [ aString ]
		ifFalse: [ self packageName, '-', aString ]
]

{ #category : #updating }
RPackageTag >> updateDefinedClassNamed: oldString withNewName: newString [
	(self hasClassNamed: oldString)
		ifFalse: [ ^ self ].
	self removeClassNamed: oldString.
	self addClassNamed: newString
]
