I am a composable presenter that  keeps reference to its domain model (announcingObject) optionally wrapped in a value holder and subscribes yourself to it.

As the model (announcingObject) we can set a subclass of Model or a value holder (NewValueHolder). In that case such models are stored directly into announcingObject.

You should implement the method #modelChanged in my subclasses
