"
This class manages the class comment and a protocol organizer
"
Class {
	#name : #ClassOrganization,
	#superclass : #Object,
	#instVars : [
		'organizedClass',
		'protocols'
	],
	#category : #'Kernel-Protocols'
}

{ #category : #'instance creation' }
ClassOrganization class >> forClass: aClass [
	^ self basicNew
		initializeClass: aClass;
		yourself
]

{ #category : #adding }
ClassOrganization >> addProtocol: aProtocol [

	^ protocols add: aProtocol
]

{ #category : #accessing }
ClassOrganization >> addProtocolNamed: protocolName [

	| oldProtocols protocol |
	(self hasProtocolNamed: protocolName) ifTrue: [ ^ self ].

	oldProtocols := self protocolNames copy.

	protocol := self addSilentlyProtocolNamed: protocolName.
	SystemAnnouncer uniqueInstance protocolAdded: protocolName inClass: self organizedClass.
	self notifyOfChangedProtocolNamesFrom: oldProtocols to: self protocolNames.
	^ protocol
]

{ #category : #accessing }
ClassOrganization >> addSilentlyProtocolNamed: protocolName [
	
	^ self addProtocol: (Protocol name: protocolName)
]

{ #category : #'backward compatibility' }
ClassOrganization >> allMethodSelectors [

	^ self protocols flatCollect: [ :p | p methodSelectors ]
]

{ #category : #'backward compatibility' }
ClassOrganization >> classComment [

	^ self comment
]

{ #category : #'backward compatibility' }
ClassOrganization >> classComment: aString [

	self comment: aString
]

{ #category : #'protocol - adding' }
ClassOrganization >> classify: aSymbol inProtocolNamed: aProtocolName [

	| protocol |
	protocol := self protocolNamed: aProtocolName ifAbsent: [
		            Smalltalk globals at: #Transcript ifPresent: [ :t |
			            t
				            show: 'Preparing to add protocol named ' , aProtocolName , ' to classify ' , aSymbol , ' into ' , organizedClass name;
				            cr ].
		            self addProtocolNamed: aProtocolName ].
	"maybe here we should check if this method already belong to another protocol"
	self protocols
		select: [ :p | p includesSelector: aSymbol ]
		thenDo: [ :p | p removeMethodSelector: aSymbol ].

	protocol addMethodSelector: aSymbol
]

{ #category : #'backward compatibility' }
ClassOrganization >> classify: selector under: aProtocol [

	| oldProtocolName forceNotify oldProtocols protocolName |
	"The next section deserve more cleanings.
	
	Some code was added to make it possible to classify giving a real protocol.
	Here to keep a small change, I just ask the name to the protocol and use that for compatibility.
	Later, I plan to update this code once more to directly use the actual object I'm givin instead of doing this little trick."
	protocolName := aProtocol isString
		                ifTrue: [ aProtocol ]
		                ifFalse: [ aProtocol name ].
	forceNotify := (self includesSelector: selector) not.
	oldProtocolName := self protocolNameOfElement: selector.
	(forceNotify or: [ oldProtocolName ~= protocolName or: [ protocolName ~= Protocol unclassified ] ]) ifFalse: [ ^ self ].
	oldProtocols := self protocolsOfSelector: selector.

	self classify: selector inProtocolNamed: protocolName.
	oldProtocols do: [ :protocol | self removeProtocolIfEmpty: protocol ].
	oldProtocolName ifNotNil: [ self notifyOfChangedSelector: selector from: oldProtocolName to: protocolName ]
]

{ #category : #cleanup }
ClassOrganization >> cleanUpProtocolsForClass: aClass [
	"remove all entries that have no methods"

	self allMethodSelectors do: [ :each | (aClass includesSelector: each) ifFalse: [ self removeElement: each ] ]
]

{ #category : #accessing }
ClassOrganization >> comment [

	^ organizedClass comment
]

{ #category : #accessing }
ClassOrganization >> comment: aString [
	organizedClass comment: aString
]

{ #category : #accessing }
ClassOrganization >> commentSourcePointer [
	^ organizedClass commentSourcePointer
]

{ #category : #accessing }
ClassOrganization >> commentSourcePointer: anObject [
	organizedClass commentSourcePointer: anObject
]

{ #category : #accessing }
ClassOrganization >> commentStamp [

	^ organizedClass commentStamp
]

{ #category : #copying }
ClassOrganization >> copyFrom: otherOrganization [

	otherOrganization protocols do: [ :protocol | protocol methodSelectors do: [ :m | self classify: m inProtocolNamed: protocol name ] ]
]

{ #category : #'queries - protocols' }
ClassOrganization >> ensureProtocol: aProtocol [
	"I can take a Protocol or a protocol name as paramater.
	
	If my parameter is a name, I'll return a protocol associated with it. A new one if needed.
	If my parameter is a Protocol, I'll ensure that it comes from me, else I'll throw an error.
	If I receive nil, I return the unclassified protocol"

	aProtocol ifNil: [ ^ self ensureProtocol: Protocol unclassified ].

	aProtocol isString ifFalse: [
		(self protocols includes: aProtocol)
			ifTrue: [ ^ aProtocol ]
			ifFalse: [ self error: 'I received a real protocol but this one is not part of me.' ] ].
	^ self protocolNamed: aProtocol ifAbsent: [ self addProtocolNamed: aProtocol ]
]

{ #category : #accessing }
ClassOrganization >> extensionProtocols [
	^ self protocols select: #isExtensionProtocol
]

{ #category : #testing }
ClassOrganization >> hasComment [
	^ organizedClass hasComment
]

{ #category : #testing }
ClassOrganization >> hasProtocolNamed: aString [

	^ self protocols anySatisfy: [ :each | each name = aString ]
]

{ #category : #testing }
ClassOrganization >> includesSelector: selector [

	^ self protocols anySatisfy: [ :each | each includesSelector: selector ]
]

{ #category : #initialization }
ClassOrganization >> initialize [

	super initialize.
	self reset
]

{ #category : #initialization }
ClassOrganization >> initializeClass: aClass [

	self initialize.
	organizedClass := aClass.
	organizedClass selectors do: [ :each | self classify: each under: Protocol unclassified ]
]

{ #category : #accessing }
ClassOrganization >> methodSelectorsInProtocol: aName [

	^ (self protocolNamed: aName ifAbsent: [ ^ #(  ) ]) methodSelectors asArray
]

{ #category : #private }
ClassOrganization >> moveMethodsFrom: fromProtocolNamed to: toProtocolNamed [

	| fromProtocol toProtocol |
	fromProtocol := self protocolNamed: fromProtocolNamed.
	toProtocol := self protocolNamed: toProtocolNamed.

	toProtocol addAllMethodsFrom: fromProtocol.
	fromProtocol removeAllMethodSelectors.

	^ toProtocol
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedProtocolNamesFrom: oldProtocols to: newProtocols [

	oldProtocols ~= newProtocols ifTrue: [ SystemAnnouncer uniqueInstance classReorganized: self organizedClass ]
]

{ #category : #notifications }
ClassOrganization >> notifyOfChangedSelector: element from: oldProtocolName to: newProtocolName [

	oldProtocolName ~= newProtocolName ifTrue: [ self organizedClass notifyOfRecategorizedSelector: element from: oldProtocolName to: newProtocolName ]
]

{ #category : #notifications }
ClassOrganization >> notifyOfRemovedProtocolNamed: protocolName [

	SystemAnnouncer uniqueInstance protocolRemoved: protocolName inClass: self organizedClass
]

{ #category : #accessing }
ClassOrganization >> organizedClass [

	^ organizedClass ifNil: [ self error: 'ClassOrganization should always have an organized class associated.' ]
]

{ #category : #accessing }
ClassOrganization >> protocolNameOfElement: aSelector [

	^ self protocolNameOfElement: aSelector ifAbsent: nil
]

{ #category : #accessing }
ClassOrganization >> protocolNameOfElement: aSelector ifAbsent: aBlock [

	^ (self protocolsOfSelector: aSelector)
		  ifEmpty: [
			  (organizedClass includesSelector: aSelector)
				  ifTrue: [ Protocol unclassified ]
				  ifFalse: [ aBlock value ] ]
		  ifNotEmpty: [ :col | col first name ]
]

{ #category : #accessing }
ClassOrganization >> protocolNamed: aString [

	^ self protocolNamed: aString ifAbsent: [ NotFound signalFor: aString ]
]

{ #category : #accessing }
ClassOrganization >> protocolNamed: aString ifAbsent: aBlock [

	^ self protocols
		  detect: [ :e | e name = aString ]
		  ifNone: aBlock
]

{ #category : #accessing }
ClassOrganization >> protocolNames [

	^ self protocols collect: [ :protocol | protocol name ]
]

{ #category : #accessing }
ClassOrganization >> protocols [

	^ protocols asArray
]

{ #category : #protocol }
ClassOrganization >> protocolsOfSelector: aSelector [

	^ (self protocols select: [:each | each includesSelector: aSelector ]) asArray
]

{ #category : #removing }
ClassOrganization >> removeElement: aSymbol [

	| oldProtocol |
	oldProtocol := self protocolNameOfElement: aSymbol.
	self protocols
		select: [ :protocol | protocol includesSelector: aSymbol ]
		thenDo: [ :p | p removeMethodSelector: aSymbol ].
	self notifyOfChangedSelector: aSymbol from: oldProtocol to: (self protocolNameOfElement: aSymbol)
]

{ #category : #removing }
ClassOrganization >> removeEmptyProtocols [

	"We copy protocols because it is usually bad to remove elements of a collection while iterating on it"
	self protocols copy do: [ :protocol | self removeProtocolIfEmpty: protocol ]
]

{ #category : #removing }
ClassOrganization >> removeProtocolIfEmpty: aProtocol [
	"I take a protocol or a protocol name and remvoe it if it is empty."

	| protocol oldProtocolNames |
	protocol := aProtocol isString
		            ifTrue: [ self protocolNamed: aProtocol ifAbsent: [ ^ self ] ]
		            ifFalse: [ aProtocol ].

	protocol isEmpty ifFalse: [ ^ self ].

	oldProtocolNames := self protocolNames copy.
	protocols remove: protocol ifAbsent: [  ].
	self notifyOfRemovedProtocolNamed: protocol name.
	self notifyOfChangedProtocolNamesFrom: oldProtocolNames to: self protocolNames
]

{ #category : #removing }
ClassOrganization >> renameProtocolNamed: oldName toBe: newName [

	(self hasProtocolNamed: oldName) ifFalse: [ ^ self ].

	self silentlyRenameProtocolNamed: oldName toBe: newName.

	"Announce the changes in the system"
	oldName ~= newName ifTrue: [
		SystemAnnouncer uniqueInstance protocolRenamedFrom: oldName to: newName inClass: self organizedClass.
		SystemAnnouncer uniqueInstance classReorganized: self organizedClass.

		"I need to notify also the selector changes, otherwise RPackage will not notice"
		(self protocolNamed: newName) methodSelectors do: [ :each | self notifyOfChangedSelector: each from: oldName to: newName ] ]
]

{ #category : #initialization }
ClassOrganization >> reset [

	protocols := IdentitySet new
]

{ #category : #'backward compatibility' }
ClassOrganization >> setSubject: anObject [

	organizedClass := anObject
]

{ #category : #private }
ClassOrganization >> silentlyRenameProtocolNamed: oldName toBe: newName [

	(self hasProtocolNamed: oldName) ifFalse: [ ^ self ].

	(self hasProtocolNamed: newName)
		ifTrue: [
			self moveMethodsFrom: oldName to: newName.
			self removeProtocolIfEmpty: oldName ]
		ifFalse: [
			^ (self protocolNamed: oldName)
				  name: newName;
				  yourself ]
]

{ #category : #'backward compatibility' }
ClassOrganization >> subject [

	self deprecated: 'Use #organizedClass instead.' transformWith: '`@rcv subject' -> '`@rcv organizedClass'.
	^ self organizedClass
]
