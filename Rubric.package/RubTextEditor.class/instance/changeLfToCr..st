editing keys
changeLfToCr: aKeyboardEvent
	"Replace all LFs by CRs.
	Triggered by Cmd-U -- useful when getting code from FTP sites
	jmv- Modified to als change crlf by cr"
	
	| fixed |
	fixed := self selection string.
	fixed := fixed copyReplaceAll: String crlf with: String cr.
	fixed := fixed copyReplaceAll: String lf with: String cr. 
	self replaceSelectionWith: (Text fromString: fixed).
	^ true