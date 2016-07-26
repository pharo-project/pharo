My instances hold a number of various attributes, used during initial window creation and its initialization:

- initial bounds
- title
- icon
- screen id
- fullscreen or not
- etc...

Please note, that depending on driver used, some attributes can be ignored by driver and will have no any effect.  

The preferableDriver attribute is special, that it allows user to bypass the default driver selection mechanism and tell directly, which driver shall be used for creating a window.