all
initialize  "CornerRounder initialize"

	CR0 := CR1 := self new
		masterMask:
			(Form extent: 6@6
				fromArray: #(2r1e26 2r111e26 2r1111e26 2r11111e26 2r11111e26 2r111111e26)
				offset: 0@0)
		masterOverlay:
			(Form extent: 6@6
				fromArray: #(2r1e26 2r110e26 2r1000e26 2r10000e26 2r10000e26 2r100000e26)
				offset: 0@0).
	CR2 := self new
		masterMask:
			(Form extent: 6@6
				fromArray: #(2r1e26 2r111e26 2r1111e26 2r11111e26 2r11111e26 2r111111e26)
				offset: 0@0)
		masterOverlay:
			(Form extent: 6@6
				fromArray: #(2r1e26 2r111e26 2r1111e26 2r11100e26 2r11000e26 2r111000e26)
				offset: 0@0).

