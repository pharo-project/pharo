Extension { #name : #BitBlt }

{ #category : #'*FreeType-Addition' }
BitBlt >> combinationRule [
	"Answer the receiver's combinationRule"
	
	^combinationRule
]

{ #category : #'*FreeType-Addition' }
BitBlt >> copyBitsColor: argbColorSmallInteger alpha: argbAlphaSmallInteger gammaTable: gammaByteArray ungammaTable: ungammaByteArray [
	"This entry point to BitBlt supplies an extra argument to specify the fore color
	argb value for operation 41. This is split into an alpha value and an rgb value,
	so that both can be passed as smallIntegers to the primitive.  
	rgbColorInteger must be a smallInteger between 0 and 16rFFFFFF.
	alpha must be a smallInteger between 0 and 16rFF."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	"Check for compressed source, destination or halftone forms"
	((sourceForm isForm) and: [sourceForm unhibernate])
		ifTrue: [^ self copyBitsColor: argbColorSmallInteger alpha: argbAlphaSmallInteger gammaTable: gammaByteArray ungammaTable: ungammaByteArray].
	((destForm isForm) and: [destForm unhibernate ])
		ifTrue: [^ self copyBitsColor: argbColorSmallInteger alpha: argbAlphaSmallInteger gammaTable: gammaByteArray ungammaTable: ungammaByteArray].
	((halftoneForm isForm) and: [halftoneForm unhibernate])
		ifTrue: [^ self copyBitsColor: argbColorSmallInteger alpha: argbAlphaSmallInteger gammaTable: gammaByteArray ungammaTable: ungammaByteArray].

	self primitiveFailed  "Later do nicer error recovery -- share copyBits recovery"
]

{ #category : #'*FreeType-Addition' }
BitBlt >> installFreeTypeFont: aFreeTypeFont foregroundColor: foregroundColor backgroundColor: backgroundColor [
	"Set up the parameters.  Since the glyphs in a TTCFont is 32bit depth form, it tries to use rule=34 to get better AA result if possible."

	(FreeTypeSettings current useSubPixelAntiAliasing and: [destForm depth >= 8])
		ifTrue:[
			self combinationRule: 41.
			destForm depth = 8
				ifTrue:[self colorMap: (self cachedFontColormapFrom: 32 to: destForm depth)]
				ifFalse:[self colorMap: nil]]
		ifFalse:[
			"use combination rule 34 when rule 41 is not available in the BitBlt plugin,
			or the destination form depth <= 8"
			destForm depth <= 8 
				ifTrue: [
					self colorMap: (self cachedFontColormapFrom: 32 to: destForm depth).
					self combinationRule: Form paint.] 
				ifFalse: [
					self colorMap: nil.
					self combinationRule: 34]].
	halftoneForm := nil.
	sourceX := sourceY := 0.
	height := aFreeTypeFont height.

]

{ #category : #'*FreeType-Addition' }
BitBlt >> lastFontForegroundColor [
	^ nil 
]
