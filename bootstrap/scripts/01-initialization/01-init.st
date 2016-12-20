#(#ISOLanguageDefinition #Delay #DangerousClassNotifier #ISO885915TextConverter #EndianDetector
#DigitalSignatureAlgorithm #ChangeSet #Float #OSPlatform #Behavior #OCASTTranslator #ExternalSemaphoreTable 
#ProcessSpecificVariable #LiteralVariable #ByteTextConverter #FileHandle 
#WeakFinalizationList #RPackageOrganizer #Slot #DateAndTime #HashTableSizes #DiskStore #SHA1 #CP1252TextConverter
#ProcessorScheduler #Categorizer #CCompilationContext #SetElement #Deprecation #ZnMimeType #TraitBehavior 
#AsyncFile #RPackage #Stdio #FileStream #ASTCache #MacRomanTextConverter 
#CompiledMethod  #ISO88592TextConverter #UUIDGenerator #Locale #Symbol
#BISimpleFormatter #InstructionStream #TemporaryVariable #MD5NonPrimitive #ChronologyConstants 
#BasicCommandLineHandler #CompilationContext #FileLocator #LanguageEnvironment #Character #SmalltalkImage 
#EncodedCharSet #ByteString #SmallInteger #String  #WeakArray #Object #Latin9Environment
 #Latin2Environment #Key #ZnBase64Encoder #UIManagerSessionHandler #MultiByteFileStream 
#Latin1  #RBScanner #CP1250TextConverter #WeakAnnouncementSubscription #ZnUTF8Encoder #Context #Unicode 
#ZnSimplifiedByteEncoder)
  do: [:cn |
    Transcript show: 'initializing class: ', cn; cr.  
    (Smalltalk at: cn) initialize].!
    
    
'From Pharo6.0 of 13 May 2016 [Latest update: #60267] on 25 October 2016 at 10:12:07.2002 am'!

!TClassDescription methodsFor: 'private' stamp: 'CamilleTeruel 9/5/2014 16:40'!
notifyRepackage: selector method: compiledMethod oldProtocol: oldProtocol newProtocol: newProtocol
	| oldPackage newPackage |
	
	(newProtocol = oldProtocol)
		ifTrue: [ ^ self ].
		
	"This indirection is because we need to abstract RPackage from the kernel"
	self environment at: #RPackage ifPresent: [ :rPackageClass | 
			newPackage := rPackageClass organizer 
				packageForProtocol: newProtocol 
				inClass: self.
			oldPackage := rPackageClass organizer 
				packageForProtocol: oldProtocol 
				inClass: self.
			
			"Announce recategorization"
			newPackage = oldPackage
				ifFalse: [ 
					SystemAnnouncer uniqueInstance 
						methodRepackaged: compiledMethod 
						from: oldPackage 
						to: newPackage ] ].	
	
	SystemAnnouncer uniqueInstance methodRecategorized: compiledMethod oldProtocol: oldProtocol! !    
    
!WorkingSession methodsFor: 'initialization' stamp: 'ChristopheDemarey 12/4/2015 11:43'!
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
    
    
'From Pharo6.0 of 13 May 2016 [Latest update: #60069] on 13 July 2016 at 2:53:22.90776 pm'!

!UnhandledError methodsFor: 'priv handling' stamp: 'ChristopheDemarey 8/14/2015 16:06'!
defaultAction

| rep |
	rep := FileStream forceNewFileNamed: 'PharoDebug.log'.
	rep nextPutAll: 'Error:'; cr.
	rep nextPutAll: exception asString; cr.
	rep nextPutAll: thisContext stack size asString.
	thisContext stack copy withIndexDo: [:stck :i |
		[rep nextPutAll: i asString; space; nextPutAll: stck asString; cr] ifError: []].
	rep close. 
	Smalltalk exitFailure
! !    





SmalltalkImage classPool at: #SpecialSelectors put: #(#+ 1 #- 1 #< 1 #> 1 #'<=' 1 #'>=' 1 #= 1 #'~=' 1 
#* 1 #/ 1 #'\\' 1 #@ 1 #bitShift: 1 #'//' 1 #bitAnd: 1 #bitOr: 1 #at: 1 #at:put: 2 #size 0 #next 0 
#nextPut: 1 #atEnd 0 #'==' 1 #class 0 #blockCopy: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0).

Class instVarNamed: #subclasses put: (Array with: ProtoObject class).
ProtoObject class classLayout slotScope parentScope: Class classLayout slotScope.

RBProgramNode classPool at: #FormatterClass put: BISimpleFormatter.

