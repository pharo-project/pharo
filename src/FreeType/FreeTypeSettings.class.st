"
Settings for FreeType
"
Class {
	#name : #FreeTypeSettings,
	#superclass : #Object,
	#instVars : [
		'gamma',
		'hinting',
		'lightHinting',
		'subPixelAntiAliasing',
		'forceAutoHinting',
		'lcdHinting',
		'lcdvHinting',
		'monoHinting',
		'bitBltSubPixelAvailable',
		'subPixelFilters',
		'forceNonSubPixelCount',
		'gammaTable',
		'gammaInverseTable'
	],
	#classVars : [
		'UpdateFontsAtImageStartup'
	],
	#pools : [
		'FT2Constants',
		'FreeTypeCacheConstants'
	],
	#classInstVars : [
		'current'
	],
	#category : #'FreeType-Settings'
}

{ #category : #'instance creation' }
FreeTypeSettings class >> current [
	current == nil ifFalse:[^current].
	^current := self new
]

{ #category : #'class initialization' }
FreeTypeSettings class >> initialize [
	SessionManager default 
		registerGuiClassNamed: self name 
		atPriority: 10.

]

{ #category : #'system startup' }
FreeTypeSettings class >> shutDown: quitting [ 
	quitting ifFalse: [ ^ self ]. 

	self current 
		clearBitBltSubPixelAvailable;
		clearForceNonSubPixelCount.
	"FreeTypeCache clearCacheOnShutdown
		ifTrue: [ FreeTypeCache clearCurrent ].	
	FT2Handle clearRegistry.
	FreeTypeFontProvider resetCurrent. 
	EmbeddedFreeTypeFontInstaller resetCurrent."
]

{ #category : #'system startup' }
FreeTypeSettings class >> startUp: resuming [
	resuming ifTrue:[ self updateFreeType ]
]

{ #category : #settings }
FreeTypeSettings class >> updateFontsAtImageStartup [
	^ UpdateFontsAtImageStartup ifNil: [UpdateFontsAtImageStartup := false]
]

{ #category : #settings }
FreeTypeSettings class >> updateFontsAtImageStartup: aBoolean [
	UpdateFontsAtImageStartup := aBoolean
]

{ #category : #'system startup' }
FreeTypeSettings class >> updateFreeType [
	self current 
		clearBitBltSubPixelAvailable;
		clearForceNonSubPixelCount.
	"Embeded fonts update always"
	FreeTypeFontProvider current updateEmbeddedFreeTypeFonts. 
	self updateFontsAtImageStartup 
		ifTrue: [ FreeTypeFontProvider current updateFromSystem ]
]

{ #category : #accessing }
FreeTypeSettings >> bitBltSubPixelAvailable [	
	"Answer true if the the subPixel combination rule is available, false otherwise.
	to test :-
	bitBltSubPixelAvailable := false. 
	FreeTypeCache current removeAll.
	World restoreMorphicDisplay
	"
	
	| form bitBlt color |
	bitBltSubPixelAvailable == nil ifFalse:[^bitBltSubPixelAvailable].
	form := Form extent: 10@10 depth: 32.
	bitBlt := GrafPort toForm: form.
	bitBlt combinationRule: 41.
	bitBlt sourceForm: (Form extent: 5@5 depth: 32).
	bitBlt destOrigin: 1@1.
	bitBlt width: 5; height: 5.
	color := Color black asNontranslucentColor pixelValueForDepth: 32.
	[bitBlt 
		copyBitsColor: (color bitAnd: 16rFFFFFF)
		alpha: (color bitAnd: 16rFF000000) >> 24 
		gammaTable: nil
		ungammaTable: nil]
	on: Error do:[:e | ^bitBltSubPixelAvailable := false].
	#toDo. "need to check that rule 41 has done the right thing, and isn't someone elses new BitBlt rule"
	^bitBltSubPixelAvailable := true
	
]

{ #category : #accessing }
FreeTypeSettings >> clearBitBltSubPixelAvailable [	
	
	bitBltSubPixelAvailable := nil.
]

{ #category : #accessing }
FreeTypeSettings >> clearForceNonSubPixelCount [
	
	forceNonSubPixelCount := nil. 
]

{ #category : #accessing }
FreeTypeSettings >> defaultSubPixelFilterRatios [
	^#((1 3 5 3 1) (1 3 5 3 1) (1 3 5 3 1))
]

{ #category : #accessing }
FreeTypeSettings >> forceAutoHinting [
	^forceAutoHinting ifNil:[forceAutoHinting := false]
]

{ #category : #accessing }
FreeTypeSettings >> forceNonSubPixelCount [
	^forceNonSubPixelCount ifNil:[forceNonSubPixelCount := 0]
]

{ #category : #accessing }
FreeTypeSettings >> forceNonSubPixelDuring: aBlock [
	forceNonSubPixelCount ifNil:[forceNonSubPixelCount := 0].
	forceNonSubPixelCount := forceNonSubPixelCount + 1.
	aBlock ensure:[forceNonSubPixelCount := forceNonSubPixelCount - 1]
]

{ #category : #accessing }
FreeTypeSettings >> gamma [
	^gamma ifNil:[gamma := 1.0] 
]

{ #category : #accessing }
FreeTypeSettings >> gammaInverseTable [
	^gammaInverseTable
]

{ #category : #accessing }
FreeTypeSettings >> gammaTable [
	^gammaTable
]

{ #category : #accessing }
FreeTypeSettings >> glyphContrast [
	^ 100 - ((self gamma  sqrt * 100) - 50)
]

{ #category : #accessing }
FreeTypeSettings >> glyphContrast: aContrastValue [
	" value between 1 and 100.
	100 is highest contrast and maps to gamma 0.25
	1 is lowest contrast and maps to gamma 2.22"
	Cursor wait showWhile: [
		| v g |
		v := (((aContrastValue asNumber) min: 100) max: 1) asFloat.
		(v closeTo: 50.0) 
			ifTrue: [g := 1.0]
			ifFalse: [g := ((100 - v) + 50 / 100.0) raisedTo: 2].
		self setGamma: g].

]

{ #category : #accessing }
FreeTypeSettings >> hinting [
	^hinting ifNil:[hinting := true]
]

{ #category : #accessing }
FreeTypeSettings >> hintingFlags [
	| answer |
	answer := 0. 	
	self hinting 
		ifTrue:[
			self forceAutoHinting ifTrue:[answer := answer bitOr: 32 "forceAutoHinting"].	
			self lightHinting ifTrue:[answer := answer bitOr: LoadTargetLight].
			self monoHinting ifTrue:[answer := answer bitOr: LoadTargetMono].
			self lcdHinting ifTrue:[answer := answer bitOr: LoadTargetLCD].
			self lcdvHinting ifTrue:[answer := answer bitOr: LoadTargetLCDV]]
		ifFalse:[answer := 2 "no hinting"].
	^answer
]

{ #category : #accessing }
FreeTypeSettings >> hintingSymbol [
	"#Full, #Light, #Normal or #None"
	^ self monoHinting 
		ifTrue: [#Full]
		ifFalse: [
			self lightHinting 
				ifTrue: [#Light]
				ifFalse: [
					self hinting 
						ifTrue: [#Normal]
						ifFalse: [#None]]]

]

{ #category : #accessing }
FreeTypeSettings >> hintingSymbol: aSymbol [
	"#Full, #Light, #Normal or #None"
	monoHinting := aSymbol = #Full.
	lightHinting := aSymbol = #Light.
	hinting := monoHinting or:[lightHinting or:[aSymbol = #Normal]].
	FreeTypeCache current removeAll.
	FreeTypeFont allSubInstances do:[:each | each clearCachedMetrics].
	Paragraph allSubInstances do:[:each | each composeAll].
	self currentWorld restoreMorphicDisplay.


]

{ #category : #accessing }
FreeTypeSettings >> lcdHinting [
	^lcdHinting ifNil:[lcdHinting := false]
]

{ #category : #accessing }
FreeTypeSettings >> lcdvHinting [
	^lcdvHinting ifNil:[lcdvHinting := false]
]

{ #category : #accessing }
FreeTypeSettings >> lightHinting [
	^lightHinting ifNil:[lightHinting := true]
]

{ #category : #accessing }
FreeTypeSettings >> monitorType [
  "#LCD or #CRT"
  ^ self subPixelAntiAliasing ifTrue: [#LCD] ifFalse: [#CRT]

]

{ #category : #accessing }
FreeTypeSettings >> monitorType: aSymbol [
	"#LCD or #CRT"
	subPixelAntiAliasing := aSymbol = #LCD.
	self currentWorld restoreMorphicDisplay.
]

{ #category : #accessing }
FreeTypeSettings >> monoHinting [
	^monoHinting ifNil:[monoHinting := false]
]

{ #category : #accessing }
FreeTypeSettings >> pretendBitBltSubPixelUnavailableDuring: aBlock [	
	"
	For testing/profiling only. 
	
	Answer true if the the subPixel combination rule is available, false otherwise.
	to test :-
	
	bitBltSubPixelAvailable := false. 
	FreeTypeCache current removeAll.
	Smalltalk isMorphic
		ifTrue:[World restoreMorphicDisplay]
		
	"
	| old |
	old := bitBltSubPixelAvailable.
	[bitBltSubPixelAvailable := false.
	FreeTypeCache current removeAll.
	aBlock value.
	] ensure:[
		bitBltSubPixelAvailable := old.
		FreeTypeCache current removeAll.]
]

{ #category : #accessing }
FreeTypeSettings >> setGamma: aFloat [

	(aFloat closeTo: self gamma)
		ifFalse:[
			gamma := aFloat.
			(gamma closeTo: 1.0)
				ifTrue:[gammaTable := gammaInverseTable := nil]
				ifFalse:[
					gammaTable := ByteArray new: 256.
					gammaInverseTable := ByteArray new: 256.
					0 to: 255 do:[:i |
						| g ug |
						g := ((i / 255.0) raisedTo: (1.0/gamma)) * 255.
						ug := ((i / 255.0) raisedTo: gamma) * 255.
						g := (g rounded min: 255) max: 0 .
						ug := (ug rounded min: 255) max: 0 .
						gammaTable at: i + 1 put: g.
						gammaInverseTable at: i + 1 put: ug]].
				self currentWorld restoreMorphicDisplay]
]

{ #category : #accessing }
FreeTypeSettings >> setSubPixelFilter: ratiosArray [
	"Set the subPixelFilters from ratiosArray.
	the ratiosArray can specify the red, green, and blue filter ratios separately.
	e.g. #((1 3 5 3 1) (1 4 7 4 1) (1 2 3 2 1))
	or, as single set of ratios e.g. #(1 3 5 3 1)" 
	
	| validArray newFilters |
	validArray := ratiosArray.
	(ratiosArray size = 5)
		ifTrue:[validArray := {ratiosArray. ratiosArray. ratiosArray}].
	newFilters := self subPixelFiltersFromRatios: validArray. 
	(newFilters = subPixelFilters)
		ifFalse:[
			subPixelFilters := newFilters.
			FreeTypeCache current removeAllForType: FreeTypeCacheGlyphLCD.
			self currentWorld restoreMorphicDisplay]
]

{ #category : #accessing }
FreeTypeSettings >> subPixelAntiAliasing [
	^ self useSubPixelAntiAliasing and: [ subPixelAntiAliasing ifNil: [ false ] ]
]

{ #category : #accessing }
FreeTypeSettings >> subPixelAntiAliasing: aBoolean [
	subPixelAntiAliasing := aBoolean
]

{ #category : #accessing }
FreeTypeSettings >> subPixelFilters [
	^subPixelFilters ifNil:[subPixelFilters := self subPixelFiltersFromRatios: self defaultSubPixelFilterRatios]
]

{ #category : #accessing }
FreeTypeSettings >> subPixelFiltersFromRatios: anArray [
	"Convert the ratios in anArray to a similar array containing the filter proportions as floats.
	Example:
	if <array3ofArrays5> = #((1 3 5 3 1) (1 3 5 3 1) (1 3 5 3 1))
	Then the answer is #(#(0.0769230769230769 0.2307692307692308 0.3846153846153846 0.2307692307692308 0.0769230769230769) #(0.0769230769230769 0.2307692307692308 0.3846153846153846 0.2307692307692308 0.0769230769230769) #(0.0769230769230769 0.2307692307692308 0.3846153846153846 0.2307692307692308 0.0769230769230769))"
	
	| r g b rRatios gRatios bRatios rsum gsum bsum rfilter gfilter bfilter blurR blurG blurB |
		
	r := "Color red luminance" 1.0 .
	g := "Color green luminance" 1.0 .
	b := "Color blue luminance"1.0 .
	blurR :=  anArray first.
	blurG := anArray second.
	blurB := anArray third.
	rRatios := blurR collect:[:i | r*i].
	gRatios := blurG collect:[:i | g*i].
	bRatios := blurB collect:[:i | b*i].
	"rRatios := 	{g*blurR first .	b*blurR second.	r*blurR third.	g*bl.	b*blur*blur }.							
	gRatios := 	{b*blur*blur.	r*blur.	g.		b*blur.	r*blur*blur}.
	bRatios :=	{r*blur*blur.	g*blur.	b.		r*blur.	g*blur*blur }."
	rsum := rRatios inject:0 into:[:t :i | t+i].
	gsum := gRatios inject:0 into:[:t :i | t+i].
	bsum := bRatios inject:0 into:[:t :i | t+i].
	rfilter := rRatios collect:[:e | e / rsum].
	gfilter := gRatios collect:[:e | e / gsum].
	bfilter := bRatios collect:[:e | e / bsum].
	^{rfilter. gfilter. bfilter}
]

{ #category : #accessing }
FreeTypeSettings >> useSubPixelAntiAliasing [
	^ self forceNonSubPixelCount == 0 and: [ self bitBltSubPixelAvailable ]
]
