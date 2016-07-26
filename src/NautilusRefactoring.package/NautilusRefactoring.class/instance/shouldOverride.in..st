option
shouldOverride: aSelector in: aClass 
	^ self confirm: ('<1s> is already defined in the <2p> hierarchy. Extracting it to an existing selector may change behavior. Do you wish to use <1s> anyway?' expandMacrosWith: aSelector with: aClass)