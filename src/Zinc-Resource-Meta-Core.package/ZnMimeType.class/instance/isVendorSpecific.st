testing
isVendorSpecific
	"Tests if the receiver is a vendor specific mime type"
	
	^ self sub beginsWith: 'vnd.'