i am the root of keyboard events hierarchy.

The scan code represents an unique number identifying a key and comes directly from OS.
Some keys can be directly translated to corresponding character(s) by OS and thus such events will carry character ~= nil.  Character translation is not reliable with this event because many characters are introduced by using multiples key strokes. For a reliable way for getting text input, it is better to listen for the OSTextInputEvent.

character - An character representation of the key.
modifiers - Extra flags telling if special keys such as ctrl, alt, cmd, etc are being held.
position - The position of the mouse cursor when the key was pressed.
scanCode - Raw keyboard scan scode.
repeat - This value is non-zero if this event was originated by a repeated key stroke.
symbol - A virtual platform independent identifier for a keyboard key. Valid values are defined in the OSKeySymbols pool dictionary.