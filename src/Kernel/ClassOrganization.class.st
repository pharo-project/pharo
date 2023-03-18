"
This class manages the class comment and a protocol organizer
"
Class {
	#name : #ClassOrganization,
	#superclass : #Object,
	#instVars : [
		'protocolOrganizer',
		'organizedClass',
		'commentSourcePointer'
	],
	#category : #'Kernel-Protocols'
}

{ #category : #'instance creation' }
ClassOrganization class >> forClass: aClass [
	^ self basicNew
		initializeClass: aClass;
		yourself
]

{ #category : #accessing }
ClassOrganization >> addProtocolNamed: protocolName [

	| oldProtocols |
	(self protocolOrganizer hasProtocolNamed: protocolName) ifTrue: [ ^ self ].

	oldProtocols := self protocolsNames copy.
	self protocolOrganizer addProtocolNamed: protocolName.
	self notifyOfAddedProtocol: protocolName.
	self notifyOfChangedProtocolNamesFrom: oldProtocols to: self protocolsNames
]

{ #category : #'backward compatibility' }
ClassOrganization >> allMethodSelectors [
	^ protocolOrganizer allMethodSelectors
]

{ #category : #accessing }
ClassOrganization >> allProtocols [
	^ self protocolOrganizer allProtocols
]

{ #category : #accessing }
ClassOrganization >> allProtocolsNames [

	^ self protocolOrganizer allProtocolsNames
]

{ #category : #'backward compatibility' }
ClassOrganization >> classComment [

	^ self comment
]

{ #category : #'backward compatibility' }
ClassOrganization >> classComment: aString [

	self comment: aString
]

{ #category : #'backward compatibility' }
ClassOrganization >> classify: aSymbol under: aProtocolName [

	^ self
		classify: aSymbol
		under: aProtocolName
		suppressIfDefault: true
]

{ #category : #'backward compatibility' }
ClassOrganization >> classify: selector under: aProtocolName suppressIfDefault: aBoolean [

	| oldProtocolName forceNotify oldProtocols |
	forceNotify := (self protocolOrganizer includesSelector: selector) not.
	oldProtocolName := self protocolNameOfElement: selector.
	(forceNotify or: [ oldProtocolName ~= aProtocolName or: [ aBoolean not or: [ aProtocolName ~= Protocol unclassified ] ] ]) ifFalse: [ ^ self ].
	oldProtocols := self protocolOrganizer protocolsOfSelector: selector.
	self protocolOrganizer classify: selector inProtocolNamed: aProtocolName.
	aBoolean ifTrue: [ (oldProtocols select: #canBeRemoved) do: [ :e | self removeProtocol: e ] ].
	oldProtocolName ifNotNil: [ self notifyOfChangedSelector: selector from: oldProtocolName to: aProtocolName ]
]

{ #category : #cleanup }
ClassOrganization >> cleanUpProtocolsForClass: aClass [
	"remove all entries that have no methods"

	self allMethodSelectors do: [ :each | (aClass includesSelector: each) ifFalse: [ self removeElement: each ] ]
]

{ #category : #accessing }
ClassOrganization >> comment [

	^ commentSourcePointer ifNil: [''] ifNotNil: [:ptr | SourceFiles sourceCodeAt: ptr ]
]

{ #category : #accessing }
ClassOrganization >> comment: aString [
	organizedClass comment: aString
]

{ #category : #accessing }
ClassOrganization >> commentSourcePointer [
	^ commentSourcePointer
]

{ #category : #accessing }
ClassOrganization >> commentSourcePointer: anObject [
	commentSourcePointer := anObject
]

{ #category : #accessing }
ClassOrganization >> commentStamp [

	^ commentSourcePointer
		  ifNil: [ '' ]
		  ifNotNil: [ SourceFiles commentTimeStampAt: commentSourcePointer]
]

{ #category : #copying }
ClassOrganization >> copyFrom: otherOrganization [

	commentSourcePointer := otherOrganization commentSourcePointer.
	otherOrganization protocols do: [ :protocol | protocol methodSelectors do: [ :m | protocolOrganizer classify: m inProtocolNamed: protocol name ] ]
]

{ #category : #accessing }
ClassOrganization >> extensionProtocols [
	^ self protocolOrganizer extensionProtocols
]

{ #category : #testing }
ClassOrganization >> hasComment [
	^ commentSourcePointer isNotNil
]

{ #category : #'backward compatibility' }
ClassOrganization >> hasSubject [

	^ organizedClass notNil
]

{ #category : #initialization }
ClassOrganization >> initialize [
	super initialize.
	protocolOrganizer := ProtocolOrganizer new
]

{ #category : #initialization }
ClassOrganization >> initializeClass: aClass [
	self initialize.
	organizedClass := aClass.
	organizedClass selectors do: [ :each |
		self classify: each under: Protocol unclassified ]
]

{ #category : #accessing }
ClassOrganization >> methodsInProtocolNamed: aName [

	^ (self protocolOrganizer methodsInProtocolNamed: aName) asArray
]

{ #category : #notifications }
ClassOrganization >> notifyOfAddedProtocol: protocolName [

	self hasSubject ifFalse: [ ^ self ].

	SystemAnnouncer uniqueInstance protocolAdded: protocolName inClass: self subject
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedProtocolNameFrom: oldNameOrNil to: newNameOrNil [

	(self hasSubject and: [ oldNameOrNil ~= newNameOrNil ]) ifTrue: [
		SystemAnnouncer uniqueInstance protocolRenamedFrom: oldNameOrNil to: newNameOrNil inClass: self subject.
		SystemAnnouncer uniqueInstance classReorganized: self subject ]
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedProtocolNamesFrom: oldProtocols to: newProtocols [

	(self hasSubject and: [ oldProtocols ~= newProtocols ]) ifTrue: [ SystemAnnouncer uniqueInstance classReorganized: self subject ]
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedSelector: element from: oldProtocolName to: newProtocolName [

	(self hasSubject and: [ oldProtocolName ~= newProtocolName ]) ifTrue: [ self subject notifyOfRecategorizedSelector: element from: oldProtocolName to: newProtocolName ]
]

{ #category : #notifications }
ClassOrganization >> notifyOfRemovedProtocolNamed: protocolName [

	self hasSubject ifFalse: [ ^ self ].

	SystemAnnouncer uniqueInstance protocolRemoved: protocolName inClass: self subject
]

{ #category : #accessing }
ClassOrganization >> organizedClass [
	^ organizedClass
]

{ #category : #'backward compatibility' }
ClassOrganization >> protocolNameOfElement: aSelector [

	^ self protocolNameOfElement: aSelector ifAbsent: nil
]

{ #category : #'backward compatibility' }
ClassOrganization >> protocolNameOfElement: aSelector ifAbsent: aBlock [

	^ (self protocolOrganizer protocolsOfSelector: aSelector)
		  ifEmpty: [
			  (organizedClass includesSelector: aSelector)
				  ifTrue: [ Protocol unclassified ]
				  ifFalse: [ aBlock value ] ]
		  ifNotEmpty: [ :col | col first name ]
]

{ #category : #accessing }
ClassOrganization >> protocolNamed: aString [
	^ self protocolOrganizer
		protocolNamed: aString
		ifAbsent: [ nil ]
]

{ #category : #private }
ClassOrganization >> protocolOrganizer [

	^ protocolOrganizer
]

{ #category : #accessing }
ClassOrganization >> protocols [
	^ self protocolOrganizer protocols
]

{ #category : #accessing }
ClassOrganization >> protocolsNames [
	^ self protocolOrganizer protocolsNames
]

{ #category : #'backward compatibility' }
ClassOrganization >> removeElement: aSymbol [

	| oldProtocol |
	oldProtocol := self protocolNameOfElement: aSymbol.
	self protocolOrganizer removeMethod: aSymbol.
	self notifyOfChangedSelector: aSymbol from: oldProtocol to: (self protocolNameOfElement: aSymbol)
]

{ #category : #'backward compatibility' }
ClassOrganization >> removeEmptyCategories [

	| oldCategories removedProtocols |
	oldCategories := self protocolOrganizer allProtocolsNames copy.

	removedProtocols := self protocolOrganizer removeEmptyProtocols.
	removedProtocols do: [ :each | self notifyOfRemovedProtocolNamed: each name ].

	self notifyOfChangedProtocolNamesFrom: oldCategories to: self protocolOrganizer allProtocolsNames
]

{ #category : #accessing }
ClassOrganization >> removeProtocol: aProtocol [

	| oldProtocolNames |
	oldProtocolNames := self protocolOrganizer allProtocolsNames copy.
	self protocolOrganizer removeProtocol: aProtocol.
	self notifyOfRemovedProtocolNamed: aProtocol name.
	self notifyOfChangedProtocolNamesFrom: oldProtocolNames to: self protocolOrganizer allProtocolsNames
]

{ #category : #accessing }
ClassOrganization >> removeProtocolIfEmpty: protocolName [
	"The protocol may already have been removed, be non empty or a special protocol which can't be removed, such as 'all'."

	(self protocolNamed: protocolName)
		ifNotNil: [ :protocol |
			(protocol isEmpty and: [ protocol canBeRemoved ])
				ifTrue: [ self removeProtocol: protocol ] ]
]

{ #category : #accessing }
ClassOrganization >> removeProtocolNamed: protocolName [

	(self protocolOrganizer hasProtocolNamed: protocolName) ifFalse: [ ^ self ].

	self removeProtocol: (self protocolNamed: protocolName)
]

{ #category : #'backward compatibility' }
ClassOrganization >> renameCategory: oldName toBe: newName [

	self silentlyRenameCategory: oldName toBe: newName.

	self notifyOfChangedProtocolNameFrom: oldName to: newName.
	"I need to notify also the selector changes, otherwise RPackage will not notice"
	(self protocolOrganizer protocolNamed: newName) methodSelectors do: [ :each | self notifyOfChangedSelector: each from: oldName to: newName ]
]

{ #category : #'backward compatibility' }
ClassOrganization >> setSubject: anObject [

	organizedClass := anObject
]

{ #category : #private }
ClassOrganization >> silentlyRenameCategory: oldName toBe: newName [

	self protocolOrganizer
		renameProtocol: oldName
		into: newName
]

{ #category : #'backward compatibility' }
ClassOrganization >> sortCategories [
	"Do nothing"
]

{ #category : #'backward compatibility' }
ClassOrganization >> subject [

	^organizedClass
]
