actions
postload: loader package: packageSpec

	| dejavu array |

	'Starting postload action' logCr.

	Smalltalk cleanOutUndeclared.
	Author fullName: self class name.

	TextConstants initialize.
		
	"Initialize Fuel"

	FLLargeIdentityHashedCollection initialize.
	FLPlatform classPool at: #Current put: FLPharo6Platform new.

	Stdio stdout nextPutAll: 'starting...'; lf.

	Stdio stdout nextPutAll: Smalltalk ui asString; lf.

	SparseLargeTable recompile. 
	
	dejavu := FLMaterializer materializeFromFileNamed: 'BitmapDejaVuSans.fuel'.

	TextConstants TextSharedInformation at: #'DefaultTextStyle' put: dejavu.
	TextConstants TextSharedInformation at: #'Bitmap DejaVu Sans' put: dejavu.
	TextConstants TextSharedInformation at: #'DefaultMultiStyle' put: dejavu.
	TextConstants TextSharedInformation at: #'DefaultFixedTextStyle' put: dejavu.

	#(TextStyle) do: [:each | (Smalltalk globals at: each) initialize. ].

	Stdio stdout nextPutAll: 'bitmap fonts loaded' asString; lf.

	#(ProgressBarMorph BalloonBezierSimulation BalloonEngineConstants BalloonEngine 
	CornerRounder  DigitalSignatureAlgorithm DummySoundSystem 
	FreeTypeCacheConstants FreeTypeCache FreeTypeSettings 
	FreeTypeSubPixelAntiAliasedGlyphRenderer FT2Constants FT2Handle GIFReadWriter
	HaloMorph CharacterScanner ImageMorph JPEGHuffmanTable
	JPEGReadStream JPEGReadWriter 
	MczInstaller MD5NonPrimitive MenuItemMorph MenuMorph  
	PNGReadWriter RxMatcher RxParser RxsPredicate
	SHA1 ShortIntegerArray SimpleEditor
	SystemProgressMorph SystemWindow TestCase TextContainer TextDiffBuilder
	ThemeIcons ThumbnailMorph TransferMorph UITheme ZnByteEncoder
	ZnConstants ZnMimeType ZnNetworkingUtils ZnServer ZnSingleThreadedServer
	) do: [:each | (Smalltalk globals at: each) initialize. ].

	BalloonMorph setBalloonColorTo: Color yellow.

	UIManager default terminateUIProcess.
	MorphicUIManager new spawnNewProcess.
	Display newDepth: 32.
	World displayWorldSafely.
	UIManager default: MorphicUIManager new.
	World displayWorldSafely.
	UIManager default uiProcess resume.
	Pharo3Theme beCurrent.
	(World windowsSatisfying: [:w | w model canDiscardEdits]) do: [:w | w delete].
	self cleanUpAfterMorphicInitialization.

	GraphicFontSettings setFontsToStyleNamed: #small.

	TextEditor initialize.
	SmalltalkEditor initialize.

	PolymorphSystemSettings showDesktopLogo: false.
	PolymorphSystemSettings showDesktopLogo: true.
	PolymorphSystemSettings desktopColor: Color white.

	Smalltalk cleanOutUndeclared.

	#(#AbstractFont #BalloonMorph #CP1253TextConverter #Clipboard #EditorFindReplaceDialogWindow #EmbeddedFreeTypeFontInstaller #FLCompiledMethodCluster #FLLargeIdentityHashedCollection #FLPlatform #FreeTypeFontProvider #GB2312 #GradientFillStyle #GreekEnvironment #ISO88597TextConverter #IconicButton #JISX0208 #JapaneseEnvironment #KOI8RTextConverter #KSX1001 #KoreanEnvironment #LogicalFont #LucidaGrandeRegular #OSEnvironment #RussianEnvironment #ScrollBar #SimplifiedChineseEnvironment "#SourceCodeProRegular #SourceSansProRegular" #StrikeFont #SystemSettingsPersistence #TabMorph #TaskbarMorph #Text #TextAction #TextConstants #TextEditor #TextStyle) do: [:each |
		 (Smalltalk globals at: each) initialize. ].

	array := Smalltalk specialObjectsArray copy.
	array at: 5 put: Bitmap.
	Smalltalk specialObjectsArray becomeForward: array.

	Author reset.
	MCMethodDefinition initializersEnabled: initializersEnabled.

	UIManager default restoreDisplay. 