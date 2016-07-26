constants
webLink
	"Return a cursor that can be used for emphasizing web links"
	"Cursor webLink showWhile: [Sensor waitButton]"
	^WebLinkCursor ifNil:[
		WebLinkCursor :=  (CursorWithMask extent: 16@16
			fromArray: #(3072 4608 4608 4608 4608 5046 4681 29257 37449 37449 32769 32769 49155 16386 24582 16380 )
			offset: -5@0) setMaskForm:
		(Form extent: 16@16 
			fromArray: (#(3072 7680 7680 7680 7680 8118 8191 32767 65535 65535 65535 65535 65535 32766 32766 16380 )  collect: [:bits | bits bitShift: 16])
			offset: 0@0)].