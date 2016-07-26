I am an abstract class.

I'm the superclass of all the adapters used to link a Spec widget model to a framework specific widget (by example ButtonModel <-> PluggableButtonMorph).

The current implementation installs my instances as dependent of the model and my changed: method propagates updates to the widget I create (via my buildWidget method). This implementation is not optimal. 

In the future my instances should just be responsible to create a widget and install all the communication between the model and the widget. I should not be a middle man. 