'From Pharo6.0 of 13 May 2016 [Latest update: #60291] on 15 November 2016 at 2:17:35.413476 pm'!

!SubscriptionRegistry methodsFor: 'accessing' stamp: 'PavelKrivanek 11/15/2016 14:16'!
subscriptions
	^ subscriptions! !

'From Pharo6.0 of 13 May 2016 [Latest update: #60291] on 15 November 2016 at 2:17:18.181841 pm'!

!SystemAnnouncer class methodsFor: 'initialization' stamp: 'PavelKrivanek 11/15/2016 14:17'!
reset

	"unregister all current weak subscriptions because it can cause a memory leak"
	announcer subscriptions subscriptions
 		select: [:each | each isKindOf: LegacyWeakSubscription] 
		thenDo: [:each | each weakRegistry remove: each subscriber ifAbsent: []]. 
	Smalltalk garbageCollect.
		
	announcer := nil.! !
	

	
'From Pharo6.0 of 13 May 2016 [Latest update: #60143] on 11 July 2016 at 10:59:22.982106 am'!

!SystemAnnouncer class methodsFor: 'initialization' stamp: 'PavelKrivanek 7/11/2016 10:51'!
restoreAllNotifications
	"self restoreAllNotifications"
	
	| collector |
	self reset.
	collector := PragmaCollector filter: [ :pragma | pragma keyword = #systemEventRegistration ].
	collector reset.
	collector do: [ :pragma |
		pragma methodClass theNonMetaClass perform: pragma methodSelector ]! !

		
FileStream stdout nextPutAll: 'Shrinking...'; lf.!

[
  | tests keep o 
    ordered notResolved foundDeps deps depsList resolveBlock hasResolved 
    delete cyclic notResolvedNames sortedByDeps unloadFirst array  |

  [ 
    World submorphs do: #delete.
    
    100 timesRepeat: [
      10 milliSecond wait.
      World doOneCycle.
      Processor yield. ].
    
    3 timesRepeat: [Smalltalk garbageCollect].
      
    UIManager default terminateUIProcess.

    " clean processes"
    (Process allInstances
      reject: [ :process | 
        {(InputEventFetcher default instVarNamed: #fetcherProcess).
        (Processor activeProcess).
        (WeakArray runningFinalizationProcess).
        (Smalltalk lowSpaceWatcherProcess).
        (Processor backgroundProcess)} includes: process ])
      do: [ :process | 
        process suspend.
        process terminate ].
    Delay restartTimerEventLoop.
    
    "(Smalltalk at: #GTExampleOrganizer) stop.
    (Smalltalk at: #GTExampleOrganizer) compile: 'methodRemoved: anAnnouncement'."
    (Smalltalk at: #GLMBrick) removeFromSystem.
    

    tests := #('AST-Interpreter-Test' 'AST-Tests-Core' 'Announcements-Tests-Core' 'AsmJit-Tests' 
      'ClassOrganizer-Tests' 'ConfigurationCommandLineHandler-Tests' 'FileSystem-Tests-Core' 
      'FileSystem-Tests-Disk' 'FileSystem-Tests-Memory' 'Gofer-Tests' 'Graphics-Tests' 
      'HelpSystem-Tests' 'Keymapping-Tests' 'Manifest-Resources-Tests' 'Manifest-Tests' 
      'Metacello-TestsCore' 'Metacello-TestsMC' 'Metacello-TestsMCCore' 'Metacello-TestsMCResources' 
      'Metacello-TestsPharo20MC' 'Metacello-TestsPlatform' 'Metacello-TestsTutorial' 
      'Multilingual-Tests' 'NativeBoost-Tests' 'OpalCompiler-Tests' 'ProfStef-Tests' 'RPackage-Tests' 
      'Refactoring-Tests-Changes' 'Refactoring-Tests-Core' 'Refactoring-Tests-Critics' 
      'Refactoring-Tests-Environment' 'Regex-Tests-Core' 'Ring-Tests-Containers' 'Ring-Tests-Kernel' 
      'Ring-Tests-Monticello' 'System-SessionManager-Tests' 'SUnit-Tests' 'SUnit-UITesting' 'Spec-Tests' 'Text-Tests' 
      'UpdateStreamer-Tests' 'Zinc-Character-Encoding-Tests' 'Zinc-Resource-Meta-Tests' 'Zinc-Tests' 
      'Zodiac-Tests' 'FuelTests' 'KernelTests' 'MorphicTests' 'NetworkTests' 'ShoutTests' 'SlotTests' 
      'Tests' 'BalloonTests' 'CollectionsTests' 'CompressionTests' #'Debugger-Tests' 'EmbeddedFreeType-Tests' 
      'FontInfrastructure-Tests' 'FreeTypeTests' 'Graphics-Fonts-Tests' 'Monticello-Tests' 'NECompletion-Tests' 
      'Nautilus-Tests' 'Reflectivity-Tests' 'ReleaseTests' #'ScriptLoader-Tests' 'Slot-Tests' 
      'SmartSuggestions-Tests' 'System-CachingTests' 'System-History-Tests' 'System-Localization-Tests' 
      'Text-Edition-Tests' 'Tool-FileList-Tests' 'ToolsTest' 'UserManager-Tests' 'Versionner-Tests-Core-Commands' 
      'Versionner-Tests-Core-DependenciesModel' 'Versionner-Tests-Resources'
      ).

    unloadFirst := #('QualityAssistantRecording' 'QualityAssistant' 'GT-Debugger').      
   
    bootstrappedPackages := #('AST-Core' 
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
    
    "'System-Object Events'" Colors).

    keep := bootstrappedPackages.

    " clean "
    
    MetacelloProjectRegistration resetRegistry.
    
    SystemAnnouncer uniqueInstance unsubscribe: EpMonitor current.
    Job jobAnnouncer unsubscribe: EpMonitor current.
    SystemAnnouncer uniqueInstance unsubscribe: Breakpoint.
       
    KMRepository reset.
    KMPragmaKeymapBuilder release.
    PragmaCollector allInstances do: [:pc | pc announcer subscriptions reset].
    RecentMessageList stopNotifications.
    NOCCompletionTable reset.
    SystemAnnouncer uniqueInstance unsubscribe: NOCCompletionTable.
    MCWorkingCopyBrowser allInstances do: [ 
        :mcwcb | SystemAnnouncer uniqueInstance unsubscribe: mcwcb].
    ZnNetworkingUtils default secureSocketStreamClass: nil.
    ZnNetworkingUtils default socketStreamClass: SocketStream.
    TextConstants TextSharedInformation removeKey: #DefaultTextStyle.
    TextConstants TextSharedInformation removeKey: #DefaultMultiStyle.
    TextConstants TextSharedInformation removeKey: #DefaultFixedTextStyle.
    TextConstants TextSharedInformation removeKey: (Smalltalk classNamed: #StrikeFont) defaultFontKey.
    Smalltalk allClasses select: [ :c | c sharedPools includes: TextConstants ] thenDo: [ :c | c compileAll ].

    " change UI manager"
    UIManager default: DummyUIManager new.

    FileStream stdout nextPutAll: 'Removing tests...'; lf.

    tests
      withIndexDo: [ :packageName :index | 
        | wc |
        wc := MCWorkingCopy forPackage: (MCPackage named: packageName).
        FileStream stdout nextPutAll: index asString , '/' , tests size asString , ' - unloading ' , wc packageName; lf.
        wc unload ].

    unloadFirst
      withIndexDo: [ :packageName :index | 
        | wc |
        wc := MCWorkingCopy forPackage: (MCPackage named: packageName).
        FileStream stdout nextPutAll: index asString , '/' , tests size asString , ' - unloading ' , wc packageName; lf.
        wc unload ].


    " count unloading order and select packages with cyclic dependencies "
    cyclic := OrderedCollection new.
    [ 
      deps := Dictionary new.
      (MCWorkingCopy allManagers collect: #packageName)
        do: [ :packageName | 
          | package |
          package := RPackageSet named: packageName.
          deps at: packageName put: (package extensionMethods 
            groupedBy: [ :m | (Smalltalk classOrTraitNamed: m className) package packageName ]) keys ].
      depsList := deps associations asOrderedCollection.
      resolveBlock := [ :a | a value isEmpty or: [ keep, cyclic includes: a key]].
      hasResolved := depsList select: resolveBlock.
      notResolved := depsList reject: resolveBlock.
      ordered := hasResolved collect: #key.

      [ foundDeps := notResolved select: [ :a | ordered includesAll: a value ].
        foundDeps removeAllSuchThat: [ :a | ((notResolved copyWithoutAll: foundDeps) gather: #value) asSet includes: a key ].
        notResolved := notResolved copyWithoutAll: foundDeps.
        ordered addAll: (foundDeps collect: #key).
        foundDeps isEmpty ] whileFalse.

      notResolvedNames := notResolved collect: [ :a | a key ].
      sortedByDeps := (notResolved collect: [ :a | a key -> (notResolved select: [ :b | b value includes: a key ]) size ])
        asSortedCollection: [ :a :b | a value <= b value ].
      sortedByDeps ifNotEmpty: [ cyclic add: sortedByDeps last key ].
      notResolvedNames isEmpty ] whileFalse.

    " sort packages with cyclic dependencies by count of dependencies "
    notResolvedNames := notResolved collect: [ :a | a key ].
    sortedByDeps := (notResolved collect: [ :a | a key -> (notResolved select: [ :b | b value includes: a key ]) size ])
      asSortedCollection: [ :a :b | a value <= b value ].

    " remove extension methods of packages with cyclic dependencies manually "
    FileStream stdout lf; nextPutAll: 'Removing messages...'; lf.
    cyclic do: [ :pn | 
        (RPackageSet named: pn) extensionMethods do: [ :m | 
            FileStream stdout nextPutAll: m printString; lf.
            (Smalltalk classOrTraitNamed: m className) removeSelector: m name ] ].

    FileStream stdout lf; nextPutAll: 'Removing regular packages...'; lf.
    
    " move packages with cyclic dependencies at the end of ordered list "
    ordered := (ordered copyWithoutAll: cyclic), cyclic reversed.

    " select packages to delete, keep counted order "
    delete := ordered copyWithoutAll: keep.

    FileStream stdout nextPutAll: 'ordered: ' , ordered asString; lf; lf.
    FileStream stdout nextPutAll: 'keep: ' , keep asString; lf; lf.
    FileStream stdout nextPutAll: 'cyclic: ' , cyclic asString; lf; lf.
    FileStream stdout nextPutAll: 'delete: ' , delete asString; lf; lf.
    
    " unload packages "
    delete withIndexDo: [ :packageName :index | 
        | wc |
        wc := MCWorkingCopy forPackage: (MCPackage named: packageName).
        FileStream stdout nextPutAll: index asString , '/' , delete size asString , ' - unloading ' , packageName; lf.
        wc unload ].

    FileStream stdout nextPutAll: 'Cleaning...'; lf.

    " clean Morphic globals "
    #(ActiveEvent World ActiveWorld ActiveHand) do: [ :sym | 
        Smalltalk at: sym put: nil.
        Smalltalk removeKey: sym ].

    " Monticello still keeps some classes (why???), remove them manually "
    o := Smalltalk globals associations select: [ :a | a value isClass ].
    o := o select: [ :a | a value package name = '_UnpackagedPackage' ].
    o := o collect: #key.
    o do: [ :cn | 
        FileStream stdout nextPutAll: 'redelete ' , cn; lf.
        (Smalltalk globals at: cn) removeFromSystem ].

    " final cleaning "
    ChangeSet removeChangeSetsNamedSuchThat: [ :each | true ].
    ChangeSet resetCurrentToNewUnnamedChangeSet.
    Author reset.
    SmalltalkImage classPool at: #Tools put: nil.
    Job jobAnnouncer initialize.
    EncodedCharSet initialize.
    LanguageEnvironment resetKnownEnvironments.
    
    RPackageOrganizer resetDefault.
    bootstrappedPackages do: [:packageName | RPackageOrganizer default registerPackageNamed: packageName].
    
    (Smalltalk at: #Display) becomeForward: nil.
    Smalltalk removeKey: #Display.

    MCWorkingCopy allInstances do: [:wc | wc repositoryGroup: nil].

    Smalltalk removeKey: #Sensor.
    MCRepositoryGroup instVarNamed: #default put: nil.
    
    "((Array allInstances select: [ :i | i size = 1024 ])
 	    detect: [ :a | a second = SmallInteger  ])  at: 54 put: nil."
 	    
 	    
	"remove obsolete bitmap"

	array := Smalltalk specialObjectsArray copy.
	array at: 5 put: nil.
	Smalltalk specialObjectsArray becomeForward: array.
	
ClassOrganization allInstancesDo: [ :org | org removeEmptyCategories ].	
SystemOrganization removeEmptyCategories.
 	    

    " clean object memory, maybe can be simpler"
    3 timesRepeat: [ Smalltalk garbageCollect ].
    Smalltalk globals flushClassNameCache.
    3 timesRepeat: [ 
        Smalltalk garbageCollect.
        Symbol compactSymbolTable ].
    HashedCollection rehashAll.
    SmalltalkImage current fixObsoleteReferences.
    3 timesRepeat: [ Smalltalk garbageCollect ].

    UIManager default progressBarEnabled: true.
    
    FileStream stdout nextPutAll: 'DONE'; lf.

    Smalltalk saveImageInNewContext. 

    ] ifError: [ :e | 
      | rep |
      rep := FileStream forceNewFileNamed: 'PharoDebug.log'.
      rep nextPutAll: 'info.st'; cr.
      rep nextPutAll: 'Error:'; cr.
      rep nextPutAll: e asString; cr.
      rep nextPutAll: thisContext stack size asString.
      thisContext stack copy
        withIndexDo: [ :stck :i | 
          [ 
          rep nextPutAll: i asString; space; nextPutAll: stck asString; cr ] ifError: [  ] ].
      rep close.
      Smalltalk exitFailure ]
] fork.
