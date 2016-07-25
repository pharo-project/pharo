I am a basic (abstract) class, which represents an OS window driver.
The driver connects an OSWindow instances with underlaying operating system window(s) through managing OSWindowHandle(s). Driver provides an implemenation of all OSWindow functionality, starting from its creation, setting/retrieving its attributes, and finishing with event handling and/or rendering window's contents on screen.

The driver connects OSWindow(s) with operating system windows by providing the handle (see OSWindowHandle). The way how various OSWindow features and API are implemented is up to the concrete driver and thus considered private.

The driver(s) responsible for initial window creation , proper setup and and managing external resources. 
Again, most of driver's functionality is considered private and application-level code should not rely on any of its features. 

Driver selection mechanism:

 - on session change, i scan all of my subclasses to pick a most suitable driver which will be used on current platform (see #current on my class side)