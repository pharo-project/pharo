bootstrap
initializeClasses
	
	self bootstrapImageEval: '#(#ISOLanguageDefinition #Delay #DangerousClassNotifier #ISO885915TextConverter #EndianDetector
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
		  do: [:cn | (Smalltalk at: cn) initialize]' save: true quit: true.