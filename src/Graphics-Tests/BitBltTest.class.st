"
SUnit tests for class BitBlt 
"
Class {
	#name : #BitBltTest,
	#superclass : #ClassTestCase,
	#category : #'Graphics-Tests-Primitives'
}

{ #category : #'tests - bugs' }
BitBltTest >> testAlphaCompositing [

	| bb f1 f2 mixColor result eps |
	f1 := Form extent: 1@1 depth: 32.
	f2 := Form extent: 1@1 depth: 32.
	eps := 0.5 / 255.
	0 to: 255 do:[:i|
		f1 colorAt: 0@0 put: Color blue.
		mixColor := Color red alpha: i / 255.0.
		f2 colorAt: 0@0 put: mixColor.
		mixColor := f2 colorAt: 0@0.
		bb := BitBlt toForm: f1.
		bb sourceForm: f2.
		bb combinationRule: Form blend.
		bb copyBits.
		result := f1 colorAt: 0@0.
		self assert: (result red - mixColor alpha) abs < eps.
		self assert: (result blue - (1.0 - mixColor alpha)) abs < eps.
		self assert: result alpha = 1.0.
	].
]

{ #category : #'tests - bugs' }
BitBltTest >> testAlphaCompositing2 [

	| bb f1 f2 mixColor result eps |
	f1 := Form extent: 1@1 depth: 32.
	f2 := Form extent: 1@1 depth: 32.
	eps := 0.5 / 255.
	0 to: 255 do:[:i|
		f1 colorAt: 0@0 put: Color transparent.
		mixColor := Color red alpha: i / 255.0.
		f2 colorAt: 0@0 put: mixColor.
		mixColor := f2 colorAt: 0@0.
		bb := BitBlt toForm: f1.
		bb sourceForm: f2.
		bb combinationRule: Form blend.
		bb copyBits.
		result := f1 colorAt: 0@0.
		self assert: (result red - mixColor alpha) abs < eps.
		self assert: result alpha = mixColor alpha.
	].
]

{ #category : #'tests - bugs' }
BitBltTest >> testAlphaCompositing2Simulated [

	Smalltalk globals
		at: #BitBltSimulation
		ifPresent: [ :bitblt | 
			| bb f1 mixColor f2 result eps |
			f1 := Form extent: 1 @ 1 depth: 32.
			f2 := Form extent: 1 @ 1 depth: 32.
			eps := 0.5 / 255.
			0 to: 255 do: [ :i | 
				f1 colorAt: 0 @ 0 put: Color transparent.
				mixColor := Color red alpha: i / 255.0.
				f2 colorAt: 0 @ 0 put: mixColor.
				mixColor := f2 colorAt: 0 @ 0.
				bb := BitBlt toForm: f1.
				bb sourceForm: f2.
				bb combinationRule: Form blend.
				result := f1 colorAt: 0 @ 0.
				self assert: (result red - mixColor alpha) abs < eps.
				self assert: result alpha = mixColor alpha ] ]
]

{ #category : #'tests - bugs' }
BitBltTest >> testAlphaCompositingSimulated [

	Smalltalk globals
		at: #BitBltSimulation
		ifPresent: [ :bitblt | 
			| mixColor result eps f1 bb f2 |
			f1 := Form extent: 1 @ 1 depth: 32.
			f2 := Form extent: 1 @ 1 depth: 32.
			eps := 0.5 / 255.
			0 to: 255 do: [ :i | 
				f1 colorAt: 0 @ 0 put: Color blue.
				mixColor := Color red alpha: i / 255.0.
				f2 colorAt: 0 @ 0 put: mixColor.
				mixColor := f2 colorAt: 0 @ 0.
				bb := BitBlt toForm: f1.
				bb sourceForm: f2.
				bb combinationRule: Form blend.
				result := f1 colorAt: 0 @ 0.
				self assert: (result red - mixColor alpha) abs < eps.
				self assert: (result blue - (1.0 - mixColor alpha)) abs < eps.
				self assert: result alpha = 1.0 ] ]
]

{ #category : #'tests - bugs' }
BitBltTest >> testPeekerUnhibernateBug [

	| bitBlt |
	bitBlt := BitBlt bitPeekerFromForm: Display.
	bitBlt destForm hibernate.
	bitBlt pixelAt: 1 @ 1
]

{ #category : #'tests - bugs' }
BitBltTest >> testPokerUnhibernateBug [

	| bitBlt |
	bitBlt := BitBlt bitPokerToForm: Display.
	bitBlt sourceForm hibernate.
	bitBlt pixelAt: 1 @ 1 put: 0
]
