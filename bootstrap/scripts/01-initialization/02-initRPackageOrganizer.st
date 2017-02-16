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
