running
setUp
	"create the prototypes for testing"
	super setUp.
	prototypes add: TextFontChange defaultFontChange.
	prototypes add: TextFontChange font1.
	prototypes add: TextFontChange font2.
	prototypes add: TextFontChange font3.
	prototypes add: TextFontChange font4.
	prototypes
		add: (TextFontChange fontNumber: 6) 