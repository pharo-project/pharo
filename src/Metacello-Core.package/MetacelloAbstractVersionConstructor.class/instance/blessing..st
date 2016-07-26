api
blessing: aBlockOrString
    "Define blessing field of version spec (MetacelloMCVersionSpec).
	 If <aBlockOrString> is a String, the version spec blessing is set to the String. It is recommended to use a Symbol.
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the blessing spec (MetacelloValueHolderSpec). Not Recommended!
	
		spec blessing: #release.
		
		spec blessing: [
			spec value: #release. ].
	
	The blessing should typically be set to one of three values:
		#baseline - indicating that the version spec is specifying a baseline version
		#development - indicating that the version spec is not stabilized and will change over time
		#release - indicating that the version spec has stabilized and will NOT change over time
	 "

    self root blessing: aBlockOrString constructor: self