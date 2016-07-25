i am like my superclass, can display any scene (object which understands #renderOn:  message)
and support free-form pan and zoom.

but in addition, i expecting that scene object understands following:

scene handleEvent: event in: sceneView at: position
 
This method should answer true if scene object wants to handle event by itself , preventing default handling of view (pan & zoom).
The point is coordinates of event in scene coordinate space (not screen coordinate space). The event is MorphicEvent (currently limited only to mouse move&button events).

Note that if scene view enters zooming or panning state, no events will be passed to scene object as long as state is active. 


