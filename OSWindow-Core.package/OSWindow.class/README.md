I am a central class, which provides a top-level API for creating/controlling an operating-system windows.

To create a new OS window with default attributes, just use

OSWindow new.

For better control of window creation, use 

OSWindow createWithAttributes: ...

message. (See OSWindowAttributes for description).

A newly created OSWindow instance can be used and controlled by application.
To handle events (mouse/keyboard) of newly created window, one must 
bind own event handler to it (#eventHandler:) which must understand #handleEvent: message.

To render on window's surface, first application must obtain an OSWindowRenderer instance.
Currently there's two kinds of renderers available:
- form renderer (to use existing form for updating window's contents)
- opengl renderer (to render using OpenGL).

OSWindow instance and its handle: 
 - all operations with window (like hiding/showing/resizing etc) is possible only if its handle is valid. If window gets destroyed, or image opened in a new session while window was created in previous session, the handle becomes invalid, and any operations will lead to error. 
To test if window is still valid, you can just use #isValid message.