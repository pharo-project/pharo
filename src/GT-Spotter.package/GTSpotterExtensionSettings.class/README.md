This is a helper class that provides dynamically setting entries for all processors defined in the image. The user can use the settings browser to turn each processor on/off.

To achieve this, the class side keeps track of disabledExtensions.

As each setting requires selectors for setting/getting values, the class side implements a doesNotUnderstand: and maintains the disabledExtensions list.