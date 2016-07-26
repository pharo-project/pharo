*FreeType-Addition
copyBitsColor: argbColorSmallInteger alpha: argbAlphaSmallInteger gammaTable: gammaByteArray ungammaTable: ungammaByteArray
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