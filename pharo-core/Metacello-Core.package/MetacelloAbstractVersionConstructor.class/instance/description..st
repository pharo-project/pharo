api
description: aBlockOrString
    "Define description field of version spec (MetacelloMCVersionSpec).
	 If <aBlockOrString> is a String, the version spec blessing is set to the String. It is recommended to use a Symbol.
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the blessing spec (MetacelloValueHolderSpec). Not Recommended!
	
		spec description: 'Descriptive comment'.
		
		spec description: [
			spec value: 'Descriptive comment'.
	 "

    self root description: aBlockOrString constructor: self