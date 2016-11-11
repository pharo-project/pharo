actions
additionalInitialization

	 Smalltalk tools register: GTGenericStackDebugger as: #debugger.

	Smalltalk tools registeredDebuggersWithRanks.
	Smalltalk tools debuggers add: (GTGenericStackDebugger->3).
	Smalltalk tools debuggers add: (GTSUnitDebugger->70).
	Smalltalk tools debuggers add: (GTBytecodeDebugger->5).

	Smalltalk tools register: GTPlayground as: #workspace.
	Smalltalk tools register: GTInspector as: #inspector.
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

	RealEstateAgent usedStrategy: #cascadeFor:initialExtent:world:.
	GrowlMorph position: #bottomLeft.
	ShortcutReminder enabled: true.

	QASettings nautilusPlugin: true.

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
	
	3 timesRepeat: [
		Smalltalk garbageCollect.
		Undeclared removeUnreferencedKeys.].