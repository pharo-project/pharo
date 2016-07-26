STONReader materializes objects using the Smalltalk Object Notation format.

This parser is backwards compatible with standard JSON.

Customization options are:

- allowComplexMapKeys <Boolean> default is false
	if true, any object is allowed as map key
	if false, only strings, symbols and numbers are allowed as map keys
- acceptUnknownClasses <Boolean> default is false
	if true, unknown class names are allowed, the standard #mapClass (Dictionary) is instanciated and the class name is added under #classNameKey (#className)
	if false, unknown class names result in a NotFound error
- convertNewLines <Boolean> default is false
	if true, any unescaped EOL sequence CR, LF or CRLF inside strings or symbols is read and converted as the chosen EOL sequence
	if false, CR, LF and CRLF are read unmodified
- newLine <String> default is String cr
	sequence to use as EOL