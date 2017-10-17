I am a composable presenter that  keeps reference to its domain model using a value holder (specCompatibleModel) and subscribes yourself to it.

As the model we can set a subclass of Model or a value holder (NewValueHolder). In that case such models are stored directly into specCompatibleModel.

You should implement the method #modelChanged in my subclasses
