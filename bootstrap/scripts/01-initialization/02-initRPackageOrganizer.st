(File named: 'protocols.txt') readStreamDo: [:binaryStream | | stream |
  stream := ZnCharacterReadStream on: binaryStream encoding: 'utf8'.
  [stream atEnd] whileFalse: [ | line items selector |
    line := stream upTo: Character cr.
    items := line findTokens: String tab.
    class := (items second = 'true') 
      ifTrue: [(Smalltalk classOrTraitNamed: items first) theMetaClass]
      ifFalse: [Smalltalk classOrTraitNamed: items first].
    selector := items third asSymbol.

    (class localSelectors includes: selector)
      ifTrue: [class organization classify: selector under: items fourth].
]].


RPackageOrganizer default basicInitializeFromPackagesList: {
	"Bootstrap packages"
	'AST-Core'.
	'Announcements-Core'.
	'Colors'.
	'Collections-Abstract'.
	'Collections-Arithmetic'.
	'Collections-Atomic'.
	'Collections-Native'.
	'Collections-Sequenceable'.
	'Collections-Stack'.
	'Collections-Streams'.
	'Collections-Strings'.
	'Collections-Support'.
	'Collections-Unordered'.
	'Collections-Weak'.
	'Files'.
	'FileSystem-Path'.
	'Hermes'.
	'Jobs'.
	'Kernel'.
	'Kernel-BytecodeEncoders'.
	'Network-UUID'.
	'NonInteractiveTranscript'.
	'PharoBootstrap-Initialization'.
	'RPackage-Core'.
	'Random-Core'.
	'Slot'.
	'System-Announcements'.
	'System-BasicCommandLineHandler'.
	'System-CommandLine'.
	'System-CommandLineHandler'.
	'System-Finalization'.
	'System-Hashing'.
	'System-Model'.
	'System-Platforms'.
	'System-SessionManager'.
	'System-Sources'.
	'System-Support'.
	'Traits'.
	'Transcript'.
	'UIManager'.
	'Zinc-Character-Encoding-Core'.
	
	"These packages are added using hermes after bootstrap"
	'CodeExport'.
    'CodeImport'.
	'CodeImportCommandLineHandlers'.
	'OpalCompiler-Core'.
	
	"These packages are added using the compiler after bootstrap"
	'FileSystem-Core'.
	'FileSystem-Disk'.
	'Deprecated70'.
}.

RPackageOrganizer registerInterestToSystemAnnouncement.
