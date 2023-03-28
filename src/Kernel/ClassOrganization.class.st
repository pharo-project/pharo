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
	(self hasProtocolNamed: protocolName) ifTrue: [ ^ self ].

	oldProtocols := self protocolNames copy.
	self protocolOrganizer addProtocolNamed: protocolName.
	self notifyOfAddedProtocol: protocolName.
	self notifyOfChangedProtocolNamesFrom: oldProtocols to: self protocolNames
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

	^ self allProtocols collect: [ :each | each name ]
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
ClassOrganization >> classify: selector under: aProtocolName [

	| oldProtocolName forceNotify oldProtocols |
	forceNotify := (self protocolOrganizer includesSelector: selector) not.
	oldProtocolName := self protocolNameOfElement: selector.
	(forceNotify or: [ oldProtocolName ~= aProtocolName or: [ aProtocolName ~= Protocol unclassified ] ]) ifFalse: [ ^ self ].
	oldProtocols := self protocolOrganizer protocolsOfSelector: selector.
	self protocolOrganizer classify: selector inProtocolNamed: aProtocolName.
	(oldProtocols select: #canBeRemoved) do: [ :e | self removeProtocol: e ].
	oldProtocolName ifNotNil: [ self notifyOfChangedSelector: selector from: oldProtocolName to: aProtocolName ]
]

{ #category : #cleanup }
ClassOrganization >> cleanUpProtocolsForClass: aClass [
	"remove all entries that have no methods"

	self allMethodSelectors do: [ :each | (aClass includesSelector: each) ifFalse: [ self removeElement: each ] ]
]

{ #category : #accessing }
ClassOrganization >> comment [

	^ commentSourcePointer
		  ifNil: [ '' ]
		  ifNotNil: [ :ptr | SourceFiles sourceCodeAt: ptr ]
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

{ #category : #testing }
ClassOrganization >> hasOrganizedClass [

	^ organizedClass isNotNil
]

{ #category : #testing }
ClassOrganization >> hasProtocolNamed: aString [

	^ self allProtocols anySatisfy: [ :each | each name = aString ]
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
	organizedClass selectors do: [ :each | self classify: each under: Protocol unclassified ]
]

{ #category : #accessing }
ClassOrganization >> methodsInProtocolNamed: aName [

	^ (self protocolOrganizer methodsInProtocolNamed: aName) asArray
]

{ #category : #notifications }
ClassOrganization >> notifyOfAddedProtocol: protocolName [

	self hasOrganizedClass ifFalse: [ ^ self ].

	SystemAnnouncer uniqueInstance protocolAdded: protocolName inClass: self organizedClass
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedProtocolNameFrom: oldNameOrNil to: newNameOrNil [

	(self hasOrganizedClass and: [ oldNameOrNil ~= newNameOrNil ]) ifFalse: [ ^ self ].
	SystemAnnouncer uniqueInstance protocolRenamedFrom: oldNameOrNil to: newNameOrNil inClass: self organizedClass.
	SystemAnnouncer uniqueInstance classReorganized: self organizedClass
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedProtocolNamesFrom: oldProtocols to: newProtocols [

	(self hasOrganizedClass and: [ oldProtocols ~= newProtocols ]) ifTrue: [ SystemAnnouncer uniqueInstance classReorganized: self organizedClass ]
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedSelector: element from: oldProtocolName to: newProtocolName [

	(self hasOrganizedClass and: [ oldProtocolName ~= newProtocolName ]) ifTrue: [ self organizedClass notifyOfRecategorizedSelector: element from: oldProtocolName to: newProtocolName ]
]

{ #category : #notifications }
ClassOrganization >> notifyOfRemovedProtocolNamed: protocolName [

	self hasOrganizedClass ifFalse: [ ^ self ].

	SystemAnnouncer uniqueInstance protocolRemoved: protocolName inClass: self organizedClass
]

{ #category : #accessing }
ClassOrganization >> organizedClass [

	^ organizedClass
]

{ #category : #accessing }
ClassOrganization >> protocolNameOfElement: aSelector [

	^ self protocolNameOfElement: aSelector ifAbsent: nil
]

{ #category : #accessing }
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

	^ self protocolOrganizer protocolNamed: aString
]

{ #category : #accessing }
ClassOrganization >> protocolNamed: aString ifAbsent: aBlock [

	self protocolOrganizer protocolNamed: aString ifAbsent: aBlock
]

{ #category : #accessing }
ClassOrganization >> protocolNames [

	^ self protocols collect: #name
]

{ #category : #private }
ClassOrganization >> protocolOrganizer [

	^ protocolOrganizer
]

{ #category : #accessing }
ClassOrganization >> protocols [
	^ self protocolOrganizer protocols
]

{ #category : #'backward compatibility' }
ClassOrganization >> removeElement: aSymbol [

	| oldProtocol |
	oldProtocol := self protocolNameOfElement: aSymbol.
	self protocolOrganizer removeMethod: aSymbol.
	self notifyOfChangedSelector: aSymbol from: oldProtocol to: (self protocolNameOfElement: aSymbol)
]

{ #category : #removing }
ClassOrganization >> removeEmptyProtocols [

	| oldProtocolNames removedProtocols |
	oldProtocolNames := self allProtocolsNames copy.

	removedProtocols := self protocolOrganizer removeEmptyProtocols.
	removedProtocols do: [ :each | self notifyOfRemovedProtocolNamed: each name ].

	self notifyOfChangedProtocolNamesFrom: oldProtocolNames to: self allProtocolsNames
]

{ #category : #accessing }
ClassOrganization >> removeProtocol: aProtocol [

	| oldProtocolNames |
	oldProtocolNames := self allProtocolsNames copy.
	self protocolOrganizer removeProtocol: aProtocol.
	self notifyOfRemovedProtocolNamed: aProtocol name.
	self notifyOfChangedProtocolNamesFrom: oldProtocolNames to: self allProtocolsNames
]

{ #category : #accessing }
ClassOrganization >> removeProtocolIfEmpty: protocolName [
	"The protocol may already have been removed, be non empty or a special protocol which can't be removed, such as 'all'."

	| protocol |
	protocol := self protocolNamed: protocolName ifAbsent: [ ^ self ].
	(protocol isEmpty and: [ protocol canBeRemoved ]) ifTrue: [ self removeProtocol: protocol ]
]

{ #category : #accessing }
ClassOrganization >> removeProtocolNamed: protocolName [

	self removeProtocol: (self protocolNamed: protocolName ifAbsent: [ ^ self ])
]

{ #category : #removing }
ClassOrganization >> renameProtocolNamed: oldName toBe: newName [

	(self hasProtocolNamed: oldName) ifFalse: [ ^ self ].

	self silentlyRenameProtocolNamed: oldName toBe: newName.

	self notifyOfChangedProtocolNameFrom: oldName to: newName.
	"I need to notify also the selector changes, otherwise RPackage will not notice"
	(self protocolNamed: newName) methodSelectors do: [ :each | self notifyOfChangedSelector: each from: oldName to: newName ]
]

{ #category : #'backward compatibility' }
ClassOrganization >> setSubject: anObject [

	organizedClass := anObject
]

{ #category : #private }
ClassOrganization >> silentlyRenameProtocolNamed: oldName toBe: newName [

	self protocolOrganizer renameProtocol: oldName into: newName
]

{ #category : #'backward compatibility' }
ClassOrganization >> subject [

	self deprecated: 'Use #organizedClass instead.' transformWith: '`@rcv subject' -> '`@rcv organizedClass'.
	^ self organizedClass
]
