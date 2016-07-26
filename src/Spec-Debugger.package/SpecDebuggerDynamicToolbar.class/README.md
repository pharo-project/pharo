A toolbar that loads its actions dynamically based on pragmas. Only actions annotated with 'self debuggingActionsPragmas' that answer true to the message #appliesToDebugger: are taken into account. 

The toolbar updates itsetf every time a new value is put in the session (even if it is the same value).  If the session is set before opening the toolbar (before a spec is created) the message #update should be sent to the toolbar.

Each debugging actions is displayed by using a SpecDebugActionButton.

