"
SUnit tests for FreeType cache
"
Class {
	#name : #FreeTypeCacheTest,
	#superclass : #TestCase,
	#instVars : [
		'cache',
		'fullCache',
		'font1',
		'font1XGlyph',
		'font1ZGlyph',
		'font1YGlyph',
		'font2',
		'font3'
	],
	#pools : [
		'FreeTypeCacheConstants'
	],
	#category : #'FreeType-Tests-Cache'
}

{ #category : #running }
FreeTypeCacheTest >> setUp [
	super setUp.
	cache := FreeTypeCache new.
	font1 := FreeTypeFont basicNew.
	font2 := FreeTypeFont basicNew.
	font3 := FreeTypeFont basicNew.
	font1XGlyph := (GlyphForm extent: 100@100 depth: 32)
		advance: 100;
		linearAdvance: 10000;
		yourself.
	font1YGlyph := (GlyphForm extent: 100@100 depth: 32)
		advance: 100;
		linearAdvance: 10000;
		yourself.
	font1ZGlyph := (GlyphForm extent: 100@100 depth: 32)
		advance: 100;
		linearAdvance: 10000;
		yourself.
	fullCache := FreeTypeCache new.
	fullCache 
		maximumSize: (10*(fullCache sizeOf: font1YGlyph))..
	1 to: 10 do:[:i |
		fullCache atFont: font1 charCode: i type: FreeTypeCacheGlyph put: font1YGlyph]. 
]

{ #category : #tests }
FreeTypeCacheTest >> testConstants [
	| constants |
	constants := {FreeTypeCacheWidth . FreeTypeCacheGlyphMono . FreeTypeCacheGlyphLCD . FreeTypeCacheGlyph}.
	self assert: constants asSet size equals: constants size.	"no 2 have same value"
	self assert: (constants detect: [ :x | x isNil ] ifNone: [  ]) isNil	"no value is nil"
]

{ #category : #tests }
FreeTypeCacheTest >> testEntriesRemovedFIFO [
	| |
	cache maximumSize: 10*(cache sizeOf: font1XGlyph).
	1 to: 10 do:[:i |
		cache 
			atFont: font1 
			charCode: (1000-i) 
			type: FreeTypeCacheGlyph 
			put: font1XGlyph].
	self validateCollections: cache.
	11 to:1000 do:[:i |
		cache 
			atFont: font1 
			charCode: (1000-i) 
			type: FreeTypeCacheGlyph 
			put: font1XGlyph.
		self validateSizes: cache.
		self validateCollections: cache.
		"i-9 to: i do:[:i2 |
			self 
				shouldnt: [cache atFont: font1 charCode: 1000-i2 type: FreeTypeCacheGlyph]
				raise: Error]."
		self 
			should: [cache atFont: font1 charCode: 1000-(i-10) type: FreeTypeCacheGlyph]
			raise: Error].  
	self validateSizes: cache.

	
]

{ #category : #tests }
FreeTypeCacheTest >> testFailedGet [
	self should: [ cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph ] raise: Error.
	self assert: (cache instVarNamed: #used) equals: 0.
	self validateSizes: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testFreeTypeCacheEntry [
	| f f2 f3 |
	f := FreeTypeCacheEntry new.
	f
		charCode: 1;
		font: font1;
		type: FreeTypeCacheGlyph;
		object: font1XGlyph.
	f2 := FreeTypeCacheEntry new.
	f2
		charCode: 2;
		font: font1;
		type: FreeTypeCacheGlyphLCD;
		object: font1XGlyph.
	f nextLink: f2.
	self assert: f ~= f2.
	self assert: f nextLink equals: f2.

	f3 := f copy.
	f3 nextLink: nil.
	self assert: f equals: f3	"equality not based on nextLink"
]

{ #category : #tests }
FreeTypeCacheTest >> testGlyphAccessIsThreadSafe [

	"This test is weak because it depends in the rendering of Morphic and Rubrik"
	"This test breaks 95% of times"	

	self timeLimit: 20 seconds.
	
	10 timesRepeat: [  
		 | sem text canvases blocky |

		sem := Semaphore new.
		text := (String loremIpsum: 25*1024).
		FreeTypeCache current removeAll.
		canvases := OrderedCollection new.
			
		blocky := [  | canvas |
			canvas := FormCanvas extent: 1000@1000.
			canvases add: canvas.
			(RubScrolledTextMorph new)
					setText: text;
					font: StandardFonts codeFont;
					bounds: (0@0 corner: canvas form extent);
					fullDrawOn: canvas.
			sem signal ].
			
		blocky forkAt: 39.
		blocky forkAt: 39.
		blocky forkAt: 39.
		
		sem 
			wait;
			wait;
			wait.	
			
		self assert: (((canvases at:1) form bits = (canvases at:2) form bits) and: [ ((canvases at:2) form bits = (canvases at:3) form bits) ]) 
	].


]

{ #category : #tests }
FreeTypeCacheTest >> testGlyphAccessIsThreadSafeWhenAskingWidth [

	"This test is weak because it depends in the rendering of Morphic and Rubrik"
	"This test breaks 95% of times"	

	self timeLimit: 20 seconds.
	
	10 timesRepeat: [  
		 | sem text canvases blocky widthAsk |

		sem := Semaphore new.
		text := (String loremIpsum: 25*1024).
		FreeTypeCache current removeAll.
		canvases := OrderedCollection new.

		widthAsk := [
			1 to: text size do: [ :i | StandardFonts codeFont widthOf: (text at: i). i % 50 = 0 ifTrue: [ Processor yield ]].
			sem signal ].

			
		blocky := [  | canvas |
			canvas := FormCanvas extent: 1000@1000.
			canvases add: canvas.
			(RubScrolledTextMorph new)
					setText: text;
					font: StandardFonts codeFont;
					bounds: (0@0 corner: canvas form extent);
					fullDrawOn: canvas.
			sem signal ].
			
		blocky forkAt: 39.
		blocky forkAt: 39.
		blocky forkAt: 39.
		widthAsk forkAt: 39.
		
		sem 
			wait;
			wait;
			wait;
			wait.	
			
		self assert: (((canvases at:1) form bits = (canvases at:2) form bits) and: [ ((canvases at:2) form bits = (canvases at:3) form bits) ]) 
	].


]

{ #category : #tests }
FreeTypeCacheTest >> testInstanceInitialization [
	self assert: (cache instVarNamed: #maximumSize) equals: FreeTypeCache defaultMaximumSize.
	self assert: (cache instVarNamed: #used) equals: 0.
	self assert: (cache instVarNamed: #fontTable) class equals: cache dictionaryClass.
	self assertEmpty: (cache instVarNamed: #fontTable).
	self assert: (cache instVarNamed: #fifo) class equals: cache fifoClass.
	self assertEmpty: (cache instVarNamed: #fifo)
]

{ #category : #tests }
FreeTypeCacheTest >> testMaximumSizeRespectedOnIfAbsentPut [
	cache maximumSize: (cache sizeOf: font1XGlyph).
	cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		ifAbsentPut: [font1XGlyph].
	self validateSizes: cache.
	self validateCollections: cache.
	cache
		atFont: font1
		charCode: $Y asInteger
		type: FreeTypeCacheGlyph
		ifAbsentPut: [font1XGlyph].
	self assert: (cache instVarNamed: #used) equals: 0.	"cache has been cleared on reaching max size"
	self validateSizes: cache.
	self validateCollections: cache.
	self should: [ cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph ] raise: Error.
	self should: [ cache atFont: font1 charCode: $Y asInteger type: FreeTypeCacheGlyph ] raise: Error
]

{ #category : #tests }
FreeTypeCacheTest >> testMaximumSizeRespectedOnPut [
	cache maximumSize: (cache sizeOf: font1XGlyph).
	cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		put: font1XGlyph.
	self validateSizes: cache.
	self validateCollections: cache.
	cache
		atFont: font1
		charCode: $Y asInteger
		type: FreeTypeCacheGlyph
		put: font1XGlyph.
	self assert: (cache instVarNamed: #used) equals: 0.	"cache has been cleared on reaching max size"
	self validateSizes: cache.
	self validateCollections: cache.
	self should: [ cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph ] raise: Error.
	self should: [ cache atFont: font1 charCode: $Y asInteger type: FreeTypeCacheGlyph ] raise: Error
]

{ #category : #tests }
FreeTypeCacheTest >> testNormalGetIfAbsentPut [
	| u g r |
	cache maximumSize: nil.
	u := cache instVarNamed: #used.
	r := cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		ifAbsentPut: [ font1XGlyph ].
	self assert: (r isKindOf: GlyphForm).
	self assert: (cache instVarNamed: #used) > u.	"grown"
	self validateSizes: cache.
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph.
	self assert: g identicalTo: font1XGlyph.
	self validateSizes: cache.
	self validateCollections: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testNormalGetIfAbsentPutTwice [
	| u g r |
	cache maximumSize: nil.
	u := cache instVarNamed: #used.
	r := cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		ifAbsentPut: [ font1XGlyph ].
	r := cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		ifAbsentPut: [ font1XGlyph ].
	self assert: (r isKindOf: GlyphForm).
	self assert: (cache instVarNamed: #used) > u.	"grown"
	self validateSizes: cache.
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph.
	self assert: g identicalTo: font1XGlyph.
	self validateSizes: cache.
	self validateCollections: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testNormalGetIfAbsentPutTwiceIntoNonEmptyCache [
	| u g r |
	cache maximumSize: nil.
	u := cache instVarNamed: #used.
	r := cache
		atFont: font1
		charCode: $Z asInteger
		type: FreeTypeCacheGlyph
		ifAbsentPut: [ font1XGlyph ].
	r := cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		ifAbsentPut: [ font1XGlyph ].
	r := cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		ifAbsentPut: [ font1XGlyph ].
	self assert: (r isKindOf: GlyphForm).
	self assert: (cache instVarNamed: #used) > u.	"grown"
	self validateSizes: cache.
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph.
	self assert: g identicalTo: font1XGlyph.
	self validateSizes: cache.
	self validateCollections: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testNormalPutGet [
	| u g |
	cache maximumSize: nil.
	u := cache instVarNamed: #used.
	cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		put: font1XGlyph.
	self assert: (cache instVarNamed: #used) > u.	"grown"
	self validateSizes: cache.
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph.
	self assert: g identicalTo: font1XGlyph.
	self validateSizes: cache.
	self validateCollections: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testNormalPutGetTwice [
	| u g |
	cache maximumSize: nil.
	u := cache instVarNamed: #used.
	cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		put: font1XGlyph.
	self assert: (cache instVarNamed: #used) > u.	"grown"
	self validateSizes: cache.
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph.
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph.
	self assert: g identicalTo: font1XGlyph.
	self validateSizes: cache.
	self validateCollections: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testNormalPutGetWidth [
	| u g |
	cache maximumSize: nil.
	u := cache instVarNamed: #used.
	cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheWidth
		put: 100.
	self assert: (cache instVarNamed: #used) > u.	"grown"
	self validateSizes: cache.
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheWidth.
	self assert: g equals: 100.
	self validateSizes: cache.
	self validateCollections: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testPutSameElementTwice [
	cache maximumSize: nil.
	cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		put: font1XGlyph.
	self assert: (cache instVarNamed: #used) equals: (cache sizeOf: font1XGlyph).
	self validateSizes: cache.
	self validateCollections: cache.
	cache
		atFont: font1
		charCode: $X asInteger
		type: FreeTypeCacheGlyph
		put: font1XGlyph.
	self assert: (cache instVarNamed: #used) equals: (cache sizeOf: font1XGlyph).
	self validateSizes: cache.
	self validateCollections: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testRemoveAll [
	| m fifo fontTable |
	m := fullCache instVarNamed: #maximumSize.
	fifo := fullCache instVarNamed: #fifo.
	fontTable := fullCache instVarNamed: #fontTable.
	fullCache removeAll.
	self assertEmpty: (fullCache instVarNamed: #fifo).
	self assertEmpty: (fullCache instVarNamed: #fontTable).
	self assert: (fullCache instVarNamed: #used) equals: 0.
	self assert: m equals: (fullCache instVarNamed: #maximumSize).
	self assert: fifo class equals: (fullCache instVarNamed: #fifo) class.
	self assert: fontTable class equals: (fullCache instVarNamed: #fontTable) class
]

{ #category : #tests }
FreeTypeCacheTest >> testRemoveAllForFont [
	| fifo |

	fullCache maximumSize: nil.	
	1 to: 100 do:[:i |
		fullCache atFont: font1 charCode: i type: 1 put: font1XGlyph].
	1 to: 100 do:[:i |
		fullCache atFont: font2 charCode: i type: 2 put: font1YGlyph].
	1 to: 100 do:[:i |
		fullCache atFont: font3 charCode: i type: 3 put: font1ZGlyph].
	fifo := fullCache instVarNamed: #fifo.
	self assert: (fifo detect:[:each | each font = font1] ifNone:[]) notNil.
	self assert: (fifo detect:[:each | each font = font2] ifNone:[]) notNil.
	self assert: (fifo detect:[:each | each font = font3] ifNone:[]) notNil.
	fullCache removeAllForFont: font1.
	self validateSizes: fullCache.
	self validateCollections: fullCache.
	fifo := (fullCache instVarNamed: #fifo).
	self assert: (fifo detect:[:each | each font = font1] ifNone:[]) isNil.
	self assert: (fifo detect:[:each | each font = font2] ifNone:[]) notNil.
	self assert: (fifo detect:[:each | each font = font2] ifNone:[]) notNil.
	
]

{ #category : #tests }
FreeTypeCacheTest >> testRemoveAllForType [
	| fifo |

	fullCache maximumSize: nil.	
	1 to: 100 do:[:i |
		fullCache atFont: font1 charCode: i type: 1 put: font1XGlyph].
	1 to: 100 do:[:i |
		fullCache atFont: font2 charCode: i type: 2 put: font1YGlyph].
	1 to: 100 do:[:i |
		fullCache atFont: font3 charCode: i type: 3 put: font1ZGlyph].
	fifo := fullCache instVarNamed: #fifo.
	self assert: (fifo detect:[:each | each type = 1] ifNone:[]) notNil.
	self assert: (fifo detect:[:each | each type = 2] ifNone:[]) notNil.
	self assert: (fifo detect:[:each | each type = 3] ifNone:[]) notNil.
	fullCache removeAllForType: 1.
	self validateSizes: fullCache.
	self validateCollections: fullCache.
	fifo := (fullCache instVarNamed: #fifo).
	self assert: (fifo detect:[:each | each type = 1] ifNone:[]) isNil.
	self assert: (fifo detect:[:each | each type = 2] ifNone:[]) notNil.
	self assert: (fifo detect:[:each | each type = 3] ifNone:[]) notNil.
	
]

{ #category : #tests }
FreeTypeCacheTest >> testReport [
	self assert: fullCache report equals: '100% Full (maximumSize: 400320 , used: 400320)'.
	fullCache maximumSize: 800640.
	self assert: fullCache report equals: '50% Full (maximumSize: 800640 , used: 400320)'.
	self assert: cache report equals: '0% Full (maximumSize: 5120000 , used: 0)'.
	cache maximumSize: nil.
	self assert: cache report equals: '0% Full (maximumSize: nil , used: 0)'
]

{ #category : #tests }
FreeTypeCacheTest >> testSetMaximumSize [
	cache maximumSize: 0.
	self assert: (cache instVarNamed: #maximumSize) equals: 0.
	cache maximumSize: 99999999999999999.
	self assert: (cache instVarNamed: #maximumSize) equals: 99999999999999999.
	cache maximumSize: nil.	"unbounded"
	self assert: (cache instVarNamed: #maximumSize) isNil.
	self validateSizes: cache.
	self validateCollections: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testSetMaximumSizeGrow [
	| u m |
	u := fullCache instVarNamed: #used.
	m := fullCache instVarNamed: #maximumSize.
	fullCache maximumSize: m * 2.	"grow"
	self assert: u equals: (fullCache instVarNamed: #used).
	self validateSizes: cache.
	self validateCollections: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testSetMaximumSizeShrink [
	| m |
	m := fullCache instVarNamed: #maximumSize.
	fullCache maximumSize: m // 2.	"shrink"
	self assert: (fullCache instVarNamed: #used) equals: 0.	"cache is cleared when used > maximumSize"
	self validateSizes: fullCache.
	self validateCollections: fullCache
]

{ #category : #tests }
FreeTypeCacheTest >> testSetMaximumSizeUnbounded [
	| u |
	u := fullCache instVarNamed: #used.
	fullCache maximumSize: nil.	"unbounded"
	self assert: u equals: (fullCache instVarNamed: #used).
	self validateSizes: cache.
	self validateCollections: cache
]

{ #category : #tests }
FreeTypeCacheTest >> testSingleton [
	self assert: FreeTypeCache current class equals: FreeTypeCache.
	self assert: FreeTypeCache current identicalTo: FreeTypeCache current
]

{ #category : #private }
FreeTypeCacheTest >> validateCollections: aFreeTypeCache [
	"check that the fifo list entries match the fontTable dict hierarchy"

	| fontTable fontTableEntries fifo lastLink |
	fontTable := aFreeTypeCache instVarNamed: #fontTable.
	fifo := aFreeTypeCache instVarNamed: #fifo.
	lastLink := fifo instVarNamed: #lastLink.
	fontTableEntries := Set new.
	fontTable keysAndValuesDo: [ :k1 :v1 | v1 keysAndValuesDo: [ :k2 :v2 | v2 keysAndValuesDo: [ :k3 :v3 | fontTableEntries add: v3 ] ] ].
	self assert: fifo size equals: fontTableEntries size.
	self assert: fifo asSet equals: fontTableEntries.
	self assert: (lastLink isNil or: [ lastLink nextLink isNil ])
]

{ #category : #private }
FreeTypeCacheTest >> validateSizes: aFreeTypeCache [
	"check that the used, maximumSize, and caches entries are valid"

	| fontTable calcSize max used |
	fontTable := aFreeTypeCache instVarNamed: #fontTable.
	used := aFreeTypeCache instVarNamed: #used.
	max := aFreeTypeCache instVarNamed: #maximumSize.
	calcSize := 0.
	fontTable do: [ :charCodeTable | charCodeTable do: [ :typeTable | typeTable do: [ :entry | calcSize := calcSize + (aFreeTypeCache sizeOf: entry object) ] ] ].
	self assert: calcSize equals: used.
	self assert: (max isNil or: [ used <= max ])
]
