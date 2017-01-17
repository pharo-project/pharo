
  MCPackageManager allSubInstancesDo: [:o | o  unregister].
SystemAnnouncer uniqueInstance unsubscribe: MCWorkingCopy!

| mcPackage classes |

"Smalltalk at: #ListOfMethods put: (SystemNavigation default allMethods collect: #asString). "

WeakArray restartFinalizationProcess.

mcPackage := 'Monticello' asPackage.
mcPackage extensionMethods do: #removeFromSystem.

classes := mcPackage classes.
classes do: #removeFromSystem.

mcPackage := 'Ring-Monticello' asPackage.
mcPackage extensionMethods do: #removeFromSystem.

classes := mcPackage classes.
classes do: #removeFromSystem.

SystemAnnouncer reset.
ChangesLog reset default. "subscribe to SystemAnnouncer"

WeakArray restartFinalizationProcess.

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

| packages |

packages := #('AST-Core' 
    'Announcements-Core' 'CodeExport' 
    'CodeImport' 'CodeImportCommandLineHandlers' 'Collections-Abstract' 
    'Collections-Arithmetic' 'Collections-Atomic' 'Collections-Native' 
    'Collections-Sequenceable' 'Collections-Stack' 'Collections-Streams'
    'Collections-Strings' 'Collections-Support' 'Collections-Unordered' 
    'Collections-Weak' 'Compression' 'FileSystem-Core' 'FileSystem-Disk'
    'Files' 'Jobs' 'Kernel' 'Monticello' 'Multilingual-Encodings' 'Multilingual-Languages'
    'Multilingual-TextConversion' 'Network-UUID' 'OpalCompiler-Core' 'Random-Core' 'RPackage-Core' 
    'Ring-Core-Containers' 'Ring-Core-Kernel' 'Ring-Monticello' 'RingChunkImporter'
    'Slot' 'System-Announcements' 'System-BasicCommandLineHandler' 'System-Changes' 
    'System-CommandLine' 'System-CommandLineHandler' 'System-Finalization'
    'System-Hashing' 'System-Localization' 'System-Model' 'System-Platforms' 
    'System-Sources' 'System-Support' 'Traits' 'Transcript' 'UIManager' 
    'Zinc-Character-Encoding-Core' 'Zinc-Resource-Meta-Core' 'NonInteractiveTranscript'
    
    'System-SessionManager'
    'PragmaCollector'
    "'System-Object Events'"
    ).
    

RPackageOrganizer instVarNamed: #default put: nil.
RPackageOrganizer resetDefault.
RPackageOrganizer default initializeFromPackagesList: packages.!




'From Pharo5.0 of 16 April 2015 [Latest update: #50746] on 3 May 2016 at 10:12:28.734425 am'!

!RPackageOrganizer class methodsFor: 'singleton' stamp: 'StephaneDucasse 6/3/2012 22:39'!
resetDefault

	"self resetDefault"
	default  ifNotNil: [
		"When an Organizer is not used anymore, we should always pay attention that these two actions are corretly done."
		default unregisterInterestToSystemAnnouncement.
		"MCWorkingCopy removeDependent: default"].
	default := self new.
	self packageClass initialize! !


'From Pharo5.0 of 16 April 2015 [Latest update: #50746] on 3 May 2016 at 10:12:49.017766 am'!

!RPackageOrganizer methodsFor: 'private' stamp: 'CamilleTeruel 7/29/2012 18:47'!
stopNotification
	"(self instVarNamed: #default) stopNotification"
	
	"pay attention that we can break the system using this method"
	
	SystemAnnouncer uniqueInstance unsubscribe: self.
	"MCWorkingCopy removeDependent: self."! !
	

'From Pharo5.0 of 16 April 2015 [Latest update: #50746] on 3 May 2016 at 5:09:46.782939 pm'!

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
"	#MCWorkingCopy asClassIfPresent: [ 
		anAnnouncer weak
			when: (Smalltalk globals at: #MCWorkingCopyCreated) 
				send: #updateAfterNewMCPackageRegistred: 
				to: self;
			when: (Smalltalk globals at: #MCWorkingCopyDeleted) 
				send: #updateAfterNewMCPackageUnregistred: 
				to: self	]."
! !

WeakArray restartFinalizationProcess.
WeakRegistry classPool at: #Default put: nil.
WeakRegistry default.

SystemAnnouncer restoreAllNotifications.


!

[ 
  Smalltalk saveImageInNewContext. 
]  fork.
