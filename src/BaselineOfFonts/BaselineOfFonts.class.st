Class {
	#name : 'BaselineOfFonts',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfFonts',
	#package : 'BaselineOfFonts'
}

{ #category : 'baselines' }
BaselineOfFonts >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec postLoadDoIt: #postload:package:.

			spec
				package: 'Fonts-Abstract';
				package: 'Fonts-Infrastructure' with: [ spec requires: #( 'Fonts-Abstract' 'Text-Core' ) ];
				package: 'Fonts-Infrastructure-Tests' with: [ spec requires: #( 'Fonts-Infrastructure' ) ];
				package: 'Text-Core' with: [ spec requires: #( 'Fonts-Abstract' ) ];
				package: 'Graphics-Fonts' with: [ spec requires: #( 'Fonts-Abstract' 'Fonts-Infrastructure' 'Text-Core' ) ].

			spec
				group: 'Core' with: #( 'Fonts-Abstract' 'Fonts-Infrastructure' 'Graphics-Fonts' );
				group: 'Tests' with: #( 'Fonts-Infrastructure-Tests' ) ]
]

{ #category : 'actions' }
BaselineOfFonts >> exportBitmapDejaVuSans [

	| familyName styleNames pointSizes family font fileName fontCopy style |

	familyName := 'Bitmap DejaVu Sans'.
	styleNames := #('Regular' 'Bold' 'Italic' 'Bold Italic').
	pointSizes := #(7 9 12 14 18 24).

	family := LogicalFontManager current allFamilies detect: [ :f | 
		f familyName = 	familyName ] ifNone: [  ].

	pointSizes do: [ :pointSize | 
		styleNames do: [ :styleName |

			style := family members detect: [ :m | m styleName = styleName ].
			font := style asLogicalFontOfPointSize: pointSize.
			font := font realFont.

			fileName := font familyName, ' ', pointSize asString, ' ', styleName.

			font glyphs writePNGFileNamed: fileName, '.png'.

			fontCopy := font copy.
			fontCopy instVarNamed: #derivativeFonts put: nil.
			fontCopy setGlyphs: fileName.

			STON put: fontCopy onStream: (fileName, '.ston') asFileReference writeStream ]].
]

{ #category : 'actions' }
BaselineOfFonts >> loadBitmapDejaVuSans [

	| dejaVu |

	dejaVu := self loadBitmapFontFamily: 'Bitmap DejaVu Sans' sizes: #(7 9 12 14 18 24).

	TextConstants TextSharedInformation at: #'DefaultTextStyle' put: dejaVu.
	TextConstants TextSharedInformation at: #'Bitmap DejaVu Sans' put: dejaVu.
	TextConstants TextSharedInformation at: #'DefaultMultiStyle' put: dejaVu.
	TextConstants TextSharedInformation at: #'DefaultFixedTextStyle' put: dejaVu.
]

{ #category : 'actions' }
BaselineOfFonts >> loadBitmapFontFamily: familyName sizes: pointSizes [
	| fonts fileName directoryReference |
	directoryReference := (Smalltalk os environment
		                       at: 'BOOTSTRAP_REPOSITORY'
		                       ifAbsent: [ FileSystem workingDirectory ])
		                      asFileReference / 'resources' / 'fonts'.
	fonts := pointSizes collect: [ :pointSize | 
		         | regular bold italic boldItalic |
		         fileName := familyName , ' ' , pointSize asString.
		         regular := STON fromStream:
			                    (directoryReference
			                     / (fileName , ' Regular.ston')) readStream.
		         regular setGlyphs: (PNGReadWriter formFromFileNamed:
				          (directoryReference / (fileName , ' Regular.png'))
					          fullName).
		         bold := STON fromStream:
			                 (directoryReference / (fileName , ' Bold.ston'))
				                 readStream.
		         bold setGlyphs: (PNGReadWriter formFromFileNamed:
				          (directoryReference / (fileName , ' Bold.png')) fullName).
		         italic := STON fromStream:
			                   (directoryReference
			                    / (fileName , ' Italic.ston')) readStream.
		         italic setGlyphs: (PNGReadWriter formFromFileNamed:
				          (directoryReference / (fileName , ' Italic.png'))
					          fullName).
		         boldItalic := STON fromStream:
			                       (directoryReference
			                        / (fileName , ' Bold Italic.ston'))
				                       readStream.
		         boldItalic setGlyphs: (PNGReadWriter formFromFileNamed:
				          (directoryReference / (fileName , ' Bold Italic.png'))
					          fullName).
		         regular instVarNamed: #derivativeFonts put: { 
				         bold.
				         italic.
				         boldItalic } , (Array new: 29).
		         bold instVarNamed: #derivativeFonts put: (Array new: 32).
		         italic instVarNamed: #derivativeFonts put: (Array new: 32).
		         boldItalic
			         instVarNamed: #derivativeFonts
			         put: (Array new: 32).
		         regular ].
	^ TextStyle fontArray: fonts
]

{ #category : 'actions' }
BaselineOfFonts >> loadBitmapSourcePro [

	| sansPro codePro |

	sansPro := self loadBitmapFontFamily: 'Bitmap Source Sans Pro' sizes: #(9 10 11 12 14 18 24).
	TextConstants TextSharedInformation at: #'Bitmap Source Sans Pro' put: sansPro.

	codePro := self loadBitmapFontFamily: 'Bitmap Source Code Pro' sizes: #(9 10 11 12 14 18 24).
	TextConstants TextSharedInformation at: #'Bitmap Source Code Pro' put: codePro.
]

{ #category : 'actions' }
BaselineOfFonts >> postload: loader package: packageSpec [

	self loadBitmapDejaVuSans.
	self loadBitmapSourcePro
]
