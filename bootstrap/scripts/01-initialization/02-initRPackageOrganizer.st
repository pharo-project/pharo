'From Pharo6.0 of 13 May 2016 [Latest update: #60267] on 25 October 2016 at 10:17:46.476076 am'!

!RPackageOrganizer methodsFor: 'system integration' stamp: 'BernardoContreras 10/24/2014 17:36'!
registerInterestToAnnouncer: anAnnouncer
	"There should be only one"
	anAnnouncer unsubscribe: self.

	anAnnouncer weak
		when: CategoryAdded send: #systemCategoryAddedActionFrom: to: self;
		when: CategoryRemoved send: #systemCategoryRemovedActionFrom: to: self;
		when: CategoryRenamed send: #systemCategoryRenamedActionFrom: to: self;
		when:  ClassAdded send: #systemClassAddedActionFrom: to: self;
		when:  ClassRecategorized send: #systemClassRecategorizedActionFrom: to: self;
		when:  ClassRemoved send: #systemClassRemovedActionFrom: to: self;
		when:  ClassRenamed send: #systemClassRenamedActionFrom: to: self;
		when:  ClassReorganized send: #systemClassReorganizedActionFrom: to: self;
		when:  MethodAdded send: #systemMethodAddedActionFrom: to: self;
		when:  MethodModified send: #systemMethodModifiedActionFrom: to: self;
		when:  MethodRecategorized send: #systemMethodRecategorizedActionFrom: to: self;
		when:  MethodRemoved send: #systemMethodRemovedActionFrom: to: self;
		when:  ProtocolAdded send: #systemProtocolAddedActionFrom: to: self;
		when:  ProtocolRemoved send: #systemProtocolRemovedActionFrom: to: self.

	self flag: #hack. "for decoupling MC"
	self class environment at: #MCWorkingCopy ifPresent: [
		anAnnouncer weak
			when: (Smalltalk globals at: #MCWorkingCopyCreated)
				send: #updateAfterNewMCPackageRegistred:
				to: self;
			when: (Smalltalk globals at: #MCWorkingCopyDeleted)
				send: #updateAfterNewMCPackageUnregistred:
				to: self	].! !
!

'From Pharo5.0 of 16 April 2015 [Latest update: #50656] on 22 March 2016 at 2:53:22.880903 pm'!

!RPackageOrganizer methodsFor: 'private registration' stamp: 'EstebanLorenzano 2/21/2014 12:56'!
basicRegisterPackage: aPackage
	"A new package is now available and declared in the receiver. Note that it is a low level implementation method since it does not deal with package contained information and does not update the related mapping tables."

	packages
		at: aPackage name asSymbol
		put: aPackage.
	"register mc package"
	"MCWorkingCopy forPackage: (MCPackage named: aPackage name)."

	^ aPackage! !

!RPackageOrganizer methodsFor: 'private registration' stamp: 'MarcusDenker 7/16/2014 17:25'!
basicUnregisterPackage: aPackage
	"Unregister the specified package from the list of registered packages. Raise the announcement RPackageUnregistered. This is a low level action. It does not unregister the back pointer from classes to packages or any other information managed by the organizer"

	packages
		removeKey: aPackage name asSymbol
		ifAbsent: [ self reportExtraRemovalOf: aPackage ].
	"unregister also mc package"
	"aPackage mcPackage
		ifNotNil: [ :mcPackage | mcPackage workingCopy unregister ]."! !


'protocols.txt' asFileReference readStreamDo: [:stream |
  [stream atEnd] whileFalse: [
    | line |
    line := stream upTo: Character cr.
      items := line findTokens: String tab.
  class := (items second = 'true') 
    ifTrue: [(Smalltalk classOrTraitNamed: items first) class]
    ifFalse: [Smalltalk classOrTraitNamed: items first].
  selector := items third asSymbol.

  (class localSelectors includes: selector)
    ifTrue: [class organization classify: selector under: items fourth]
    ifFalse: []..

]].


RPackageOrganizer default basicInitializeFromPackagesList: #(
    'Kernel' 'Collections-Strings' 'Collections-Weak'  'System-Support' 'Files'
    'AST-Core'
    'Announcements-Core' 'CodeExport'
    'CodeImport' 'CodeImportCommandLineHandlers' 'Collections-Abstract'
    'Collections-Arithmetic' 'Collections-Atomic' 'Collections-Native'
    'Collections-Sequenceable' 'Collections-Stack' 'Collections-Streams'
     'Collections-Support' 'Collections-Unordered'
     'FileSystem-Core' 'FileSystem-Disk'
     'Jobs'   'Multilingual-Encodings' 'Multilingual-Languages'
    'Multilingual-TextConversion' 'Network-UUID' 'OpalCompiler-Core' 'Random-Core' 'RPackage-Core'
    'Ring-Core-Containers' 'Ring-Core-Kernel' 'RingChunkImporter'
    'Slot' 'System-Announcements' 'System-BasicCommandLineHandler' 'System-Changes'
    'System-CommandLine' 'System-CommandLineHandler' 'System-Finalization'
    'System-Hashing' 'System-Localization' 'System-Model' 'System-Platforms'
    'System-Sources'  'Traits' 'Transcript' 'UIManager'
    'Zinc-Character-Encoding-Core' 'Zinc-Resource-Meta-Core' 'NonInteractiveTranscript'
    'System-SessionManager'
    'PharoBootstrap-Initialization' 'Colors'
).

RPackageOrganizer registerInterestToSystemAnnouncement.
