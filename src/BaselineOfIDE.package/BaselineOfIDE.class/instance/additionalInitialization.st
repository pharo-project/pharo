actions
additionalInitialization

	self class environment at: #GTGenericStackDebugger ifPresent: [
	  Smalltalk tools register: GTGenericStackDebugger as: #debugger.
	  Smalltalk tools registeredDebuggersWithRanks.
	  Smalltalk tools debuggers add: ((self class environment at: #GTGenericStackDebugger)->3).
	  Smalltalk tools debuggers add: ((self class environment at: #GTSUnitDebugger)->70).
	  Smalltalk tools debuggers add: ((self class environment at: #GTBytecodeDebugger)->5).].

	  
	self class environment at: #GTPlayground ifPresent: [:playground | 
	  Smalltalk tools register: playground as: #workspace.].
	self class environment at: #GTInspector ifPresent: [:inspector | 
	  Smalltalk tools register: inspector as: #inspector.].

	  Smalltalk tools register: ExternalChangesBrowser as: #changeList.
	Smalltalk tools register: FileList as: #fileList.
	Smalltalk tools register: Finder as: #finder.
	Smalltalk tools register: ProcessBrowser as: #processBrowser.
	Smalltalk tools register: RecentMessageList as: #recentMessageList.
	Smalltalk tools register: SyntaxErrorDebugger as: #syntaxErrorDebugger.
	Smalltalk tools register: TimeProfiler as: #timeProfiler.

	(MorphicCoreUIManager classPool at: #UIProcess) ifNotNil: [ :proc | proc terminate ].
	MorphicCoreUIManager classPool at: #UIProcess put: nil.

	PolymorphSystemSettings desktopColor:  Color veryVeryLightGray lighter.
	SourceCodeFonts setSourceCodeFonts: 10.
	FreeTypeSystemSettings loadFt2Library: true.
	FreeTypeSettings current monitorType: #LCD.
	FreeTypeSettings current glyphContrast: 55.
	
	CatalogSettings displayCatalogProjectsInSpotter: false.

	RealEstateAgent usedStrategy: #cascadeFor:initialExtent:world:.
	GrowlMorph position: #bottomLeft.
	ShortcutReminder enabled: true.

	self class environment at: #QASettings ifPresent: [:qaSettings |
	  qaSettings nautilusPlugin: true].

	KMRepository reset.
	Morph shortcutsHandler: KMShortcutHandler new.

	"because of Komitter"
	MCSaveVersionDialog previousMessages add: String new.

	RBProgramNode formatterClass: BIConfigurableFormatter.

	Color flushCache.

	ASTTransformExamplePluginActive recompile.
	PharoCommandLineHandler recompile.
	SmalltalkImage recompile.
	
	RubCharacterScanner initialize.
	
	RubAbstractTextArea highlightMessageSend: true.
	
	Pharo3DarkTheme beCurrent.
	
	Smalltalk ui theme settings secondarySelectionColor: (Color r: 0.31 g: 0.31 b: 0.36 alpha: 1.0).
	
	3 timesRepeat: [
		Smalltalk garbageCollect.
		Undeclared removeUnreferencedKeys.].
	
	self loadIceberg.