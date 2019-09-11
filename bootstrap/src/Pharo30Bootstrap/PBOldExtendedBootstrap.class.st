Class {
	#name : #PBOldExtendedBootstrap,
	#superclass : #PBBootstrap,
	#category : 'Pharo30Bootstrap'
}

{ #category : #bootstrap }
PBOldExtendedBootstrap >> bootstrap [ 

	self
		prepareBootstrap;
		createImage;
		initializeImage.
]

{ #category : #'image-management' }
PBOldExtendedBootstrap >> bootstrapImageDo: arguments [ 
	OSSUnixSubprocess new
		command: 'pharo-vm/pharo';
		arguments: { '--nodisplay'. imageReference fullName }, arguments;
		redirectStdout;
		redirectStderr;
		runAndWaitPollingEvery: 50 milliSeconds
		retrievingStreams: true 
		onExitDo: [ :process :outString :errString  |
			process isSuccess
				ifTrue: [ ^ outString ]
				ifFalse: [ self error: outString , errString ] ]
]

{ #category : #'image-management' }
PBOldExtendedBootstrap >> bootstrapImageEval: aString [
	
	^ self bootstrapImageEval: aString save: false quit: false
]

{ #category : #'image-management' }
PBOldExtendedBootstrap >> bootstrapImageEval: aString save: save quit: quit [
	
	| arguments |
	arguments := { 'eval' } asOrderedCollection.
	save ifTrue: [ arguments addLast: '--save' ].
	quit ifTrue: [ arguments addLast: '--quit' ].
	arguments add: aString.
	^ self bootstrapImageDo: arguments asArray
]

{ #category : #'image-management' }
PBOldExtendedBootstrap >> bootstrapImageEvalFile: aString [
	
	^ self bootstrapImageEvalFile: aString save: false quit: false
]

{ #category : #'image-management' }
PBOldExtendedBootstrap >> bootstrapImageEvalFile: aString save: save quit: quit [
	
	| arguments |
	arguments := { 'st' . aString } asOrderedCollection.
	save ifTrue: [ arguments addLast: '--save' ].
	quit ifTrue: [ arguments addLast: '--quit' ].
	^ self bootstrapImageDo: arguments asArray
]

{ #category : #monticello }
PBOldExtendedBootstrap >> bootstrapMonticello [
	
	self bootstrapImageEvalFile: 'Monticello.st' save: true quit: true.
]

{ #category : #bootstrap }
PBOldExtendedBootstrap >> initializeClasses [
	
	self bootstrapImageEval: '#(#ISOLanguageDefinition #Delay #DangerousClassNotifier #ISO885915TextConverter #EndianDetector
		#DigitalSignatureAlgorithm #ChangeSet #Float #OSPlatform #Behavior #OCASTTranslator #ExternalSemaphoreTable 
		#ProcessSpecificVariable #LiteralVariable #ByteTextConverter #FileHandle 
		#WeakFinalizationList #RPackageOrganizer #Slot #DateAndTime #HashTableSizes #DiskStore #SHA1 #CP1252TextConverter
		#ProcessorScheduler #Categorizer #CCompilationContext #SetElement #Deprecation #ZnMimeType #TraitBehavior 
		#AsyncFile #RPackage #Stdio #FileStream #ASTCache #MacRomanTextConverter 
		#CompiledMethod  #ISO88592TextConverter #UUIDGenerator #Locale #Symbol
		#BISimpleFormatter #TemporaryVariable #MD5NonPrimitive #ChronologyConstants 
		#BasicCommandLineHandler #CompilationContext #FileLocator #LanguageEnvironment #Character #SmalltalkImage 
		#EncodedCharSet #ByteString #String  #WeakArray #Object #Latin9Environment
		 #Latin2Environment #Key #ZnBase64Encoder #UIManagerSessionHandler #MultiByteFileStream 
		#Latin1  #RBScanner #CP1250TextConverter #WeakAnnouncementSubscription #ZnUTF8Encoder #Context #Unicode 
		#ZnSimplifiedByteEncoder)
		  do: [:cn | (Smalltalk at: cn) initialize]' save: true quit: true.
]

{ #category : #bootstrap }
PBOldExtendedBootstrap >> initializeImage [
	
	['script.st' asFileReference writeStreamDo: [ :stream |
		stream nextPutAll: '#(#ISOLanguageDefinition #Delay #DangerousClassNotifier #ISO885915TextConverter #EndianDetector
#DigitalSignatureAlgorithm #ChangeSet #Float #OSPlatform #Behavior #OCASTTranslator #ExternalSemaphoreTable 
#ProcessSpecificVariable #LiteralVariable #ByteTextConverter #FileHandle 
#WeakFinalizationList #RPackageOrganizer #Slot #DateAndTime #HashTableSizes #DiskStore #SHA1 #CP1252TextConverter
#ProcessorScheduler #Categorizer #CCompilationContext #SetElement #Deprecation #ZnMimeType #TraitBehavior 
#AsyncFile #RPackage #Stdio #FileStream #ASTCache #MacRomanTextConverter 
#CompiledMethod  #ISO88592TextConverter #UUIDGenerator #Locale #Symbol
#BISimpleFormatter #TemporaryVariable #MD5NonPrimitive #ChronologyConstants 
#BasicCommandLineHandler #CompilationContext #FileLocator #LanguageEnvironment #Character #SmalltalkImage 
#EncodedCharSet #ByteString #String  #WeakArray #Object #Latin9Environment
 #Latin2Environment #Key #ZnBase64Encoder #UIManagerSessionHandler #MultiByteFileStream 
#Latin1  #RBScanner #CP1250TextConverter #WeakAnnouncementSubscription #ZnUTF8Encoder #Context #Unicode 
#ZnSimplifiedByteEncoder)
  do: [:cn |
    (Smalltalk at: cn) initialize].!
    
    
''From Pharo6.0 of 13 May 2016 [Latest update: #60267] on 25 October 2016 at 10:12:07.2002 am''!

!WorkingSession methodsFor: ''initialization'' stamp: ''ChristopheDemarey 12/4/2015 11:43''!
initialize
	super initialize.
	deferredStartupActions := OrderedCollection new.
	creationTime := DateAndTime now.
	id := DateAndTime now hash.! !    

		
SessionManager classPool at: #Default put: SessionManager new.
		
SessionManager default installNewSession.


SessionManager default
		registerSystemClassNamed: #SmallInteger atPriority: 10; "system - 10"
		registerSystemClassNamed: #Delay atPriority: 20; "System - 20"
		registerSystemClassNamed: #ProcessorScheduler atPriority: 30; "System - 30"
		registerSystemClassNamed: #OSPlatform atPriority: 50; "System - 50"

		registerSystemClassNamed: #FileLocator atPriority: 90; "System - 90"
		
		registerSystemClassNamed: #Stdio; "System"
		registerSystemClassNamed: #LanguageEnvironment	; "System"
		registerSystemClassNamed: #DiskStore; "System"
		registerSystemClassNamed: #SmalltalkImage; "System"
		registerSystemClassNamed: #WeakFinalizationList; "System"
		registerSystemClassNamed: #Symbol; "System"
		registerSystemClassNamed: #Locale; "System"
		registerSystemClassNamed: #MultiByteFileStream; "System"
		registerSystemClassNamed: #WeakArray; "System"
		registerSystemClassNamed: #FileStream; "System"
		registerSystemClassNamed: #BasicCommandLineHandler; "System"
		registerSystemClassNamed: #NonInteractiveTranscript; "System"
		registerSystemClassNamed: #ASTCache; "System"
		registerSystemClassNamed: #EndianDetector. "System"
    
    
    !
    
    
''From Pharo6.0 of 13 May 2016 [Latest update: #60069] on 13 July 2016 at 2:53:22.90776 pm''!

!UnhandledError methodsFor: ''priv handling'' stamp: ''ChristopheDemarey 8/14/2015 16:06''!
defaultAction

| rep |
	rep := FileStream forceNewFileNamed: ''PharoDebug.log''.
	rep nextPutAll: ''Error:''; cr.
	rep nextPutAll: exception asString; cr.
	rep nextPutAll: thisContext stack size asString.
	thisContext stack copy withIndexDo: [:stck :i |
		[rep nextPutAll: i asString; space; nextPutAll: stck asString; cr] ifError: []].
	rep close. 
	Smalltalk exitFailure
! !    

SmalltalkImage classPool at: #SpecialSelectors put: #(#+ #- #< #> #'<=' #'>=' #= #'~=' #* #/ #'\\' #@ #bitShift: #'//' #bitAnd: #bitOr: #at: #at:put: #size #next #nextPut: #atEnd #'==' nil "class" #'~~' #value #value: #do: #new #new: #x #y).

Class instVarNamed: #subclasses put: (Array with: ProtoObject class).
ProtoObject class classLayout slotScope parentScope: Class classLayout slotScope.

RBProgramNode classPool at: #FormatterClass put: BISimpleFormatter.' ].

		self bootstrapImageEvalFile: 'script.st' save: true quit: true.
	] ensure: ['script.st' asFileReference ensureDelete]
]

{ #category : #bootstrap }
PBOldExtendedBootstrap >> initializeRPackages [
	
	['script.st' asFileReference writeStreamDo: [ :stream |
		stream nextPutAll: '''From Pharo5.0 of 16 April 2015 [Latest update: #50656] on 22 March 2016 at 2:53:22.880903 pm''!

!RPackageOrganizer methodsFor: ''private registration'' stamp: ''EstebanLorenzano 2/21/2014 12:56''!
basicRegisterPackage: aPackage
	"A new package is now available and declared in the receiver. Note that it is a low level implementation method since it does not deal with package contained information and does not update the related mapping tables."
	
	packages 
		at: aPackage name asSymbol 
		put: aPackage.
	"register mc package"
	"MCWorkingCopy forPackage: (MCPackage named: aPackage name)."
	
	^ aPackage! !

!RPackageOrganizer methodsFor: ''private registration'' stamp: ''MarcusDenker 7/16/2014 17:25''!
basicUnregisterPackage: aPackage
	"Unregister the specified package from the list of registered packages. Raise the announcement RPackageUnregistered. This is a low level action. It does not unregister the back pointer from classes to packages or any other information managed by the organizer"
	
	packages
		removeKey: aPackage name asSymbol
		ifAbsent: [ self reportExtraRemovalOf: aPackage ].
	"unregister also mc package"
	"aPackage mcPackage 
		ifNotNil: [ :mcPackage | mcPackage workingCopy unregister ]."! !	
	

''protocols.txt'' asFileReference readStreamDo: [:stream |
  [stream atEnd] whileFalse: [
    | line |
    line := stream upTo: Character cr.
      items := line findTokens: String tab.
  items logCr.
  class := (items second = ''true'') 
    ifTrue: [(Smalltalk classOrTraitNamed: items first) class]
    ifFalse: [Smalltalk classOrTraitNamed: items first].
  selector := items third asSymbol.
 
  (class localSelectors includes: selector)
    ifTrue: [class organization classify: selector under: items fourth]
    ifFalse: []..
  
]].


RPackageOrganizer default basicInitializeFromPackagesList: #(
    ''Kernel'' ''Collections-Strings'' ''Collections-Weak''  ''System-Support'' ''Files''
    ''AST-Core'' 
    ''Announcements-Core'' ''CodeExport'' 
    ''CodeImport'' ''CodeImportCommandLineHandlers'' ''Collections-Abstract'' 
    ''Collections-Arithmetic'' ''Collections-Atomic'' ''Collections-Native'' 
    ''Collections-Sequenceable'' ''Collections-Stack'' ''Collections-Streams''
     ''Collections-Support'' ''Collections-Unordered'' 
     ''FileSystem-Core'' ''FileSystem-Disk''
     ''Jobs''   ''Multilingual-Encodings'' ''Multilingual-Languages''
    ''Multilingual-TextConversion'' ''Network-UUID'' ''OpalCompiler-Core'' ''Random-Core'' ''RPackage-Core'' 
    ''Ring-Definitions-Containers'' ''Ring-Definitions-Core'' ''Ring-OldChunkImporter''
    ''Slot'' ''System-Announcements'' ''System-BasicCommandLineHandler'' ''System-Changes'' 
    ''System-CommandLine'' ''System-CommandLineHandler'' ''System-Finalization''
    ''System-Hashing'' ''System-Localization'' ''System-Model'' ''System-Platforms'' 
    ''System-Sources''  ''Traits'' ''Transcript'' ''UIManager'' 
    ''Zinc-Character-Encoding-Core'' ''Zinc-Resource-Meta-Core'' ''NonInteractiveTranscript''
    ''System-SessionManager''
    ''PharoBootstrap-Initialization'' ''Colors''
).

RPackageOrganizer registerInterestToSystemAnnouncement.'. ].
		self bootstrapImageEvalFile: 'script.st' save: true quit: true
	] ensure: ['script.st' asFileReference ensureDelete]

]

{ #category : #bootstrap }
PBOldExtendedBootstrap >> initializeSessionManager [

	[	'script.st' asFileReference writeStreamDo: [ :str |
		str nextPutAll: '''From Pharo6.0 of 13 May 2016 [Latest update: #60267] on 25 October 2016 at 10:12:07.2002 am''!
	
!WorkingSession methodsFor: ''initialization'' stamp: ''ChristopheDemarey 12/4/2015 11:43''!
initialize
	super initialize.
	deferredStartupActions := OrderedCollection new.
	creationTime := DateAndTime now.
	id := DateAndTime now hash.! !    

		
SessionManager classPool at: #Default put: SessionManager new.
SessionManager default installNewSession.

SessionManager default
		registerSystemClassNamed: #SmallInteger atPriority: 10; "system - 10"
		registerSystemClassNamed: #Delay atPriority: 20; "System - 20"
		registerSystemClassNamed: #ProcessorScheduler atPriority: 30; "System - 30"
		registerSystemClassNamed: #OSPlatform atPriority: 50; "System - 50"
		registerSystemClassNamed: #FileLocator atPriority: 90; "System - 90"
		registerSystemClassNamed: #Stdio; "System"
		registerSystemClassNamed: #LanguageEnvironment	; "System"
		registerSystemClassNamed: #DiskStore; "System"
		registerSystemClassNamed: #SmalltalkImage; "System"
		registerSystemClassNamed: #WeakFinalizationList; "System"
		registerSystemClassNamed: #Symbol; "System"
		registerSystemClassNamed: #Locale; "System"
		registerSystemClassNamed: #MultiByteFileStream; "System"
		registerSystemClassNamed: #WeakArray; "System"
		registerSystemClassNamed: #FileStream; "System"
		registerSystemClassNamed: #BasicCommandLineHandler; "System"
		registerSystemClassNamed: #NonInteractiveTranscript; "System"
		registerSystemClassNamed: #ASTCache; "System"
		registerSystemClassNamed: #EndianDetector. "System"'].
	
		self bootstrapImageEvalFile: 'script.st' save: true quit: true.
	] ensure: [ 'script.st' asFileReference ensureDelete ]
]

{ #category : #bootstrap }
PBOldExtendedBootstrap >> patchUnhandledErrorManagement [

	[	'script.st' asFileReference writeStreamDo: [ :str |
		str nextPutAll: '''From Pharo6.0 of 13 May 2016 [Latest update: #60267] on 25 October 2016 at 10:12:07.2002 am''!
	
!UnhandledError methodsFor: ''priv handling'' stamp: ''ChristopheDemarey 8/14/2015 16:06''!
defaultAction

| rep |
	rep := FileStream forceNewFileNamed: ''PharoDebug.log''.
	rep nextPutAll: ''Error:''; cr.
	rep nextPutAll: exception asString; cr.
	rep nextPutAll: thisContext stack size asString.
	thisContext stack copy withIndexDo: [:stck :i |
		[rep nextPutAll: i asString; space; nextPutAll: stck asString; cr] ifError: []].
	rep close. 
	Smalltalk exitFailure
! !'].
	
		self bootstrapImageEvalFile: 'script.st' save: true quit: true.
	] ensure: [ 'script.st' asFileReference ensureDelete ]
]

{ #category : #preparation }
PBOldExtendedBootstrap >> prepareBootstrap [
	
	self
		exportKernelProtocols;
		exportKernelPackageNames;
		exportAllPackagesIntoMcz;
		exportMonticelloInStFile.
]
