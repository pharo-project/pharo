accessing
httponly
	"This is not an RFC cookie attribute, but a supplement found in MS Internet Explorer and supported in PHP.
	 See http://msdn.microsoft.com/en-us/library/ms533046.aspx"
	^ attributes at: 'httponly' ifAbsent: [ false ]