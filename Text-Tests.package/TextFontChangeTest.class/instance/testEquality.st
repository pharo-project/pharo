tests
testEquality
	"Check that different instances of the same TextFontChange are equal"
	self assert: TextFontChange defaultFontChange = TextFontChange defaultFontChange.
	self assert: TextFontChange font1 = TextFontChange font1.
	self assert: TextFontChange font2 = TextFontChange font2.
	self assert: TextFontChange font3 = TextFontChange font3.
	self assert: TextFontChange font4 = TextFontChange font4.
	self assert: (TextFontChange fontNumber: 6)
			= (TextFontChange fontNumber: 6)