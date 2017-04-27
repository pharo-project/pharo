I am the object responsible of managing how sessions work in Pharo.
A session defines the boundaries of work in the image.

A new session starts when the image starts.
A session stops when the image quits.
There is only one active session at a single point of time.

The current active session is held by myself, the singleton session manager. It can be accessed by doing:

  SessionManager default currentSession.

The most important responsibility of the session manager is to manage how resources and services in the image are started up and shut down at the beginning and end of a session respectively. For example, when the image starts, several initialization routines should be executed to make sure that the image has access to the graphic drivers, the standard input/output file descriptors and so on.

Such initialization happens in the #snapshot:andQuit: method. #snapshot:andQuit: will:
 - stop current session
 - save current image if requested
 - quit if requested
 - start a new session
 
When a session is started, all elements registered in the startup list are started up.
When a session is stopped, all elements registered in the shutdown list are shut down.

# Managing Startup and Shutdown lists

The startup and shutdown lists can be accessed through the messages:

    SessionManager default startupList.
    SessionManager default shutdownList.

In general terms, the shutdown list is the startup list reversed.

Upon a startup [shutdown], all elements in the startup list are sent the message #startup: [#shutdown:] with a boolean as argument that indicates wether the image is being saved [closed].

Internally, startup and shutdown lists are prioritised. Priorities are managed by startup categories. By default the session manager includes the following categories in decreasing priority order:

- System
- Network
- Graphical User Interface
- Tools
- User

Categories can be accessed as follows:

    SessionManager default categoryNamed: aName.

New categories can be registered in the system using the messages:

    SessionManager default createCategory: aCategoryName.
    SessionManager default createCategory: aCategoryName after: anotherCategoryName.

Finally, to subscribe some resource handler to the startup shutdown lists, we need to subscribe a handler, subclass of AbstractSessionHandler.
The most common handler implementation so far is the ClassSessionHandler, that allows to subscribe a class for startup and shutdown, keeping backwards compatibility to the old startup mechanism.

    ClassSessionHandler forClassNamed: aClassName

We can register a session handler as follows

    SessionManager default
        register: (ClassSessionHandler forClassNamed: self name)
        inCategory: SessionManager default systemCategory.
        
Or alternatively, by talking to the corresponding category:

    SessionManager default systemCategory register: (ClassSessionHandler forClassNamed: self name)

# System Category Priorities

A system category internally prioritizes its elements to provide a fine grained control on the startup and shutdown order.
All methods above have variants that allow developers to specify the priority inside the category:  

    SessionManager default
        register: (ClassSessionHandler forClassNamed: self name)
        inCategory: SessionManager default systemCategory
        atPriority: 100.

    SessionManager default systemCategory
        register: (ClassSessionHandler forClassNamed: self name)
        atPriority: 100
        
By default, if no priority is specified, a default priority is used. Every category answers to the message #defaultPriority.

# How does an image restart from the point it was before

An important point in the image startup is how does it manage to restart from the point where it was executing when it was saved.

When the image is saved, using the snapshot primitive, the entire image is freezed at the point of the snapshot.
More particularly, the process that invoked the snapshot primitive is freezed at the point of the primitive call.
This works as a process fork: the running image will return from the snapshot primitive and the saved file will also start from the freezed point.
To differentiate whether we are executing in the running image or in the freshly-saved image, the snapshot primitive returns a boolean indicating it.

Read the comment of #snapshotPrimitive for more details.