This is the main class used to implement the exception handling system (EHS).  It plays two distinct roles:  that of the exception, and that of the exception handler.  More specifically, it implements the bulk of the protocols laid out in the ANSI specification - those protocol names are reflected in the message categories.

Exception is an abstract class.  Instances should neither be created nor trapped.  In most cases, subclasses should inherit from Error or Notification rather than directly from Exception.

Exceptions have an optional #messageText that can be set when they are signaled.
Exceptions also have the concept of #signaler, the object that is the subject of the exception.
This will be set automatically (to the #receiver), but can be set when the exception is signaled. 

In implementing this EHS, The Fourth Estate Inc. incorporated some ideas and code from Craig Latta's EHS.  His insights were crucial in allowing us to implement Context>>valueUninterruptably (and by extension, #ensure: and #ifCurtailed:), and we imported the following methods with little or no modification:

Context>>terminateTo:
Context>>terminate
Context>>receiver:
Context>>answer:

Thanks, Craig!