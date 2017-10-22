keymapping
modifiedCharacter
    self flag: #hack.
    "Hack me.  When Ctrl is pressed, the key ascii value is not right and we have to do something ugly"
    ^(self hasSpecialCTRLKeyValue and: [ (#(MacOSX Windows) includes: Smalltalk os current platformFamily) ])
        ifTrue: [ (self keyValue + $a asciiValue - 1) asCharacter ]    
        ifFalse: [ self keyCharacter ]