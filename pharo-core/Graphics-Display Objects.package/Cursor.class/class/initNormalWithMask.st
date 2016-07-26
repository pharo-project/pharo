initialization - private
initNormalWithMask    "Cursor initNormalWithMask.  Cursor normal show"
	"Next two lines work simply for any cursor..."
	self initNormal.
	NormalCursor := CursorWithMask derivedFrom: NormalCursor.

	"But for a good looking cursor, you have to tweak things..."
	NormalCursor := (CursorWithMask extent: 16 @ 16 depth: 1
			fromArray: #( 0 1073741824 1610612736 1879048192
				2013265920 2080374784 2113929216 2130706432
				2080374784 2080374784 1275068416 100663296
				100663296 50331648 50331648 0)
			offset: -1 @ -1)
		setMaskForm: (Form extent: 16@16 depth: 1
			fromArray: #( 3221225472 3758096384 4026531840 4160749568
				4227858432 4261412864 4278190080 4286578688
				4278190080 4261412864 4261412864 3472883712
				251658240 125829120 125829120 50331648)
			offset: 0 @ 0).