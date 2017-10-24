I am an abstract class.

I'm the superclass of all the adapters used to link a Spec widget presenter to a framework specific widget (by example ButtonPresenter <-> PluggableButtonMorph).

The current implementation installs my instances as dependent of the presenter and my changed: method propagates updates to the widget I create (via my buildWidget method). This implementation is not optimal. 

In the future my instances should just be responsible to create a widget and install all the communication between the presenter and the widget. I should not be a middle man. 