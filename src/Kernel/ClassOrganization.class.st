"
This object is in charge of system notifications.
It manages the class comment, the class comment stamp and a protocol organizer
"
Class {
	#name : #ClassOrganization,
	#superclass : #Object,
	#instVars : [
		'commentStamp',
		'protocolOrganizer',
		'organizedClass',
		'commentRemoteString'
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
ClassOrganization >> addCategory: aString [
	| oldCategories |
	
	(self protocolOrganizer hasProtocolNamed: aString)
		ifTrue: [ ^self ].

	oldCategories := self categories copy.
	self protocolOrganizer addProtocolNamed: aString.
	self notifyOfAddedCategory: aString.
	self 
		notifyOfChangedCategoriesFrom: oldCategories 
		to: self categories.
]

{ #category : #'backward compatibility' }
ClassOrganization >> addCategory: aProtocolName before: aUselessArgument [
	self addCategory: aProtocolName
]

{ #category : #accessing }
ClassOrganization >> allCategories [
	^ self protocolOrganizer allProtocolsNames 
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
	^ self allProtocols collect: #name
]

{ #category : #accessing }
ClassOrganization >> categories [
	^ self protocolOrganizer protocolsNames
]

{ #category : #accessing }
ClassOrganization >> categoriesSorted [

	^ self protocolOrganizer protocolsSorted
]

{ #category : #'backward compatibility' }
ClassOrganization >> categoryOfElement: aSelector [

	^ self categoryOfElement: aSelector ifAbsent: [ nil ]
]

{ #category : #'backward compatibility' }
ClassOrganization >> categoryOfElement: aSelector ifAbsent: aBlock [

	^ (self protocolOrganizer protocolsOfSelector: aSelector)
		ifEmpty: [ 
			(organizedClass includesSelector: aSelector)
				ifTrue: [ Protocol unclassified ]
				ifFalse: aBlock ]
		ifNotEmpty: [:col | col first name ]
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
ClassOrganization >> classComment: aString stamp: aStamp [

	self
		comment: aString;
		commentStamp: aStamp
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
	| oldCategory forceNotify oldProtocols |
	forceNotify := (self protocolOrganizer includesSelector: selector) not.
	oldCategory := self categoryOfElement: selector.
	(forceNotify or: [ oldCategory ~= aProtocolName or: [ aBoolean not or: [ aProtocolName ~= Protocol unclassified ] ] ])
		ifFalse: [ ^ self ].
	oldProtocols := self protocolOrganizer protocolsOfSelector: selector.
	self protocolOrganizer
		classify: selector
		inProtocolNamed: aProtocolName.		
	aBoolean ifTrue: [
		(oldProtocols 
			select: #canBeRemoved)
			do: [:e | self  removeProtocol: e ] ].
	oldCategory ifNotNil: [ self notifyOfChangedSelector: selector from: oldCategory to: aProtocolName ]
]

{ #category : #cleanup }
ClassOrganization >> cleanUpCategoriesForClass: aClass [
	"remove all entries that have no methods"
	 self allMethodSelectors do: [:each | 
		(aClass includesSelector: each) ifFalse: [self removeElement: each ]].
]

{ #category : #accessing }
ClassOrganization >> comment [

	commentRemoteString ifNil: [^ ''].
	^ commentRemoteString string ifNil: ['']
]

{ #category : #accessing }
ClassOrganization >> comment: aString [
	"Store the comment, aString, associated with the object that refers to the 
	receiver."

	commentRemoteString := aString isRemoteString
		ifTrue: [ aString ]
		ifFalse: [ 
			aString isEmptyOrNil
				ifTrue: [ nil ]
				ifFalse: [ SourceFiles remoteStringForNewString: aString ] ]
]

{ #category : #'backward compatibility' }
ClassOrganization >> commentRemoteString [

	^ commentRemoteString
]

{ #category : #accessing }
ClassOrganization >> commentStamp [
	^ commentStamp
]

{ #category : #accessing }
ClassOrganization >> commentStamp: anObject [
	commentStamp := anObject
]

{ #category : #accessing }
ClassOrganization >> extensionProtocols [ 
	^ self protocolOrganizer extensionProtocols.
]

{ #category : #testing }
ClassOrganization >> hasComment [
	^ self commentRemoteString isNotNil
]

{ #category : #'backward compatibility' }
ClassOrganization >> hasSubject [

	^ organizedClass notNil
]

{ #category : #initialization }
ClassOrganization >> initialize [ 
	super initialize.
	protocolOrganizer := ProtocolOrganizer new.
]

{ #category : #initialization }
ClassOrganization >> initializeClass: aClass [
	self initialize.
	organizedClass := aClass.
	organizedClass selectors do: [ :each | 
		self classify: each under: Protocol unclassified ]
]

{ #category : #testing }
ClassOrganization >> isEmptyCategoryNamed: categoryName [
	^ (self protocolOrganizer protocolNamed: categoryName) isEmpty
]

{ #category : #accessing }
ClassOrganization >> listAtCategoryNamed: aName [		
	^ (self protocolOrganizer methodsInProtocolNamed: aName) asArray
]

{ #category : #'backward compatibility' }
ClassOrganization >> listAtCategoryNumber: aSmallInteger [ 
	
	^ (protocolOrganizer allProtocols at: aSmallInteger ifAbsent: [ ^ {} ]) methods asArray
]

{ #category : #notifications }
ClassOrganization >> notifyOfAddedCategory: protocolName [
	
	self hasSubject ifFalse: [ ^ self ].
	
	SystemAnnouncer uniqueInstance 
		protocolAdded: protocolName 
		inClass: self subject
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedCategoriesFrom: oldCollectionOrNil to: newCollectionOrNil [
	(self hasSubject and: [ oldCollectionOrNil ~= newCollectionOrNil ]) 
		ifTrue: [ SystemAnnouncer uniqueInstance classReorganized: self subject ].
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedCategoryFrom: oldNameOrNil to: newNameOrNil [
	(self hasSubject and: [oldNameOrNil ~= newNameOrNil]) 
		ifTrue: [
			SystemAnnouncer uniqueInstance 
				protocolRenamedFrom: oldNameOrNil 
				to: newNameOrNil 
				inClass: self subject.
			SystemAnnouncer uniqueInstance classReorganized: self subject].
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedSelector: element from: oldCategory to: newCategory [
	(self hasSubject and: [(oldCategory ~= newCategory)]) 
		ifTrue: [
			self subject 
				notifyOfRecategorizedSelector: element 
				from: oldCategory 
				to: newCategory ].
]

{ #category : #notifications }
ClassOrganization >> notifyOfRemovedCategory: protocolName [
	
	self hasSubject ifFalse: [ ^ self ].
	
	SystemAnnouncer uniqueInstance 
		protocolRemoved: protocolName 
		inClass: self subject
]

{ #category : #accessing }
ClassOrganization >> organizedClass [
	^ organizedClass
]

{ #category : #'backward compatibility' }
ClassOrganization >> ownCategories [
        "get all categories that are not obtained from traits. Order is random"

        | traitsCategories |

        traitsCategories := self subject traits
                inject: Set new
                into: [:set :trait |
                        set addAll: (trait organization protocolOrganizer protocols
                                reject: [ :eachProtocol |
                                        self subject methods anySatisfy: [ :eachMethod |
                                                eachMethod origin = self subject
                                                                and: [ eachMethod protocol = eachProtocol name ] ] ]).
                        set ].

        ^ ((self protocolOrganizer protocols collect: #name)
                copyWithoutAll: (traitsCategories collect: #name)) asArray.
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
	^ self protocolOrganizer protocols.
]

{ #category : #accessing }
ClassOrganization >> removeCategory: protocolName [
	(self protocolOrganizer hasProtocolNamed: protocolName)
		ifFalse: [ ^ self ].
	
	self removeProtocol: (self protocolNamed: protocolName)
]

{ #category : #'backward compatibility' }
ClassOrganization >> removeElement: aSymbol [
	| oldProtocol |
	
	oldProtocol := self categoryOfElement: aSymbol.
	self protocolOrganizer removeMethod: aSymbol.
	self notifyOfChangedSelector: aSymbol from: oldProtocol to: (self categoryOfElement: aSymbol).
]

{ #category : #'backward compatibility' }
ClassOrganization >> removeEmptyCategories [
	| oldCategories removedProtocols |
	
	oldCategories := self protocolOrganizer allProtocolsNames copy.
	
	removedProtocols := self protocolOrganizer removeEmptyProtocols.	
	removedProtocols do: [ :each |  self notifyOfRemovedCategory: each name ].
	
	self notifyOfChangedCategoriesFrom: oldCategories to: self protocolOrganizer allProtocolsNames.
]

{ #category : #accessing }
ClassOrganization >> removeProtocol: aProtocol [
	| oldCategories |
	
	oldCategories := self protocolOrganizer allProtocolsNames copy.
	self protocolOrganizer removeProtocol: aProtocol.
	self notifyOfRemovedCategory: aProtocol name.
	self 
		notifyOfChangedCategoriesFrom: oldCategories 
		to: self protocolOrganizer allProtocolsNames.
]

{ #category : #accessing }
ClassOrganization >> removeProtocolIfEmpty: protocolName [
	"The protocol may already have been removed, be non empty or a special protocol which can't be removed, such as 'all'."

	(self protocolNamed: protocolName)
		ifNotNil: [ :protocol | 
			(protocol isEmpty and: [ protocol canBeRemoved ])
				ifTrue: [ self removeProtocol: protocol ] ]
]

{ #category : #'backward compatibility' }
ClassOrganization >> renameCategory: oldName toBe: newName [
	
	self 
		silentlyRenameCategory: oldName 
		toBe: newName.

	self notifyOfChangedCategoryFrom: oldName to: newName.
	"I need to notify also the selector changes, otherwise RPackage will not notice"
	(self protocolOrganizer protocolNamed: newName) methodSelectors 
		do: [ :each | self notifyOfChangedSelector: each from: oldName to: newName ]
]

{ #category : #'backward compatibility' }
ClassOrganization >> setSubject: anObject [
	
	organizedClass := anObject
]

{ #category : #private }
ClassOrganization >> silentlyRenameCategory: oldName toBe: newName [

	self protocolOrganizer 
		renameProtocol: oldName
		into: newName.

]

{ #category : #'backward compatibility' }
ClassOrganization >> sortCategories [
	"Do nothing"
]

{ #category : #'backward compatibility' }
ClassOrganization >> subject [

	^organizedClass
]
