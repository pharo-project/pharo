tests - bugs
testAlphaCompositing2Simulated
	"self run: #testAlphaCompositing2Simulated"

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