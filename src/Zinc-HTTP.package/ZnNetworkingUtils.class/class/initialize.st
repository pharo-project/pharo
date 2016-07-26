class initialization
initialize
	"The default will try to use ZdcSocketStream and ZdcSecureSocketStream"
	 
	Default := self new.
	DefaultSocketStreamTimeout := 30