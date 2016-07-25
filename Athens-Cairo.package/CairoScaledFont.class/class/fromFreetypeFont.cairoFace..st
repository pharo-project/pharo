instance creation
fromFreetypeFont: aFont cairoFace: face

	| options fontMatrix deviceMatrix font |

	options := CairoBackendCache soleInstance 
		at: #AAOptions ifAbsentPut: [ CairoFontOptions new ].
	fontMatrix := AthensCairoMatrix new.
	deviceMatrix := AthensCairoMatrix new.

	fontMatrix scaleBy: (TextStyle pointsToPixels: aFont pointSize).
	
	font := self primCreate: face fontMatrix: fontMatrix  userToDeviceMatrix: deviceMatrix options: options. 
	
	"to keep a reference to cairo face in instance"
	^ font initWithFace: face.
	
