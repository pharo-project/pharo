My subclasses (not instances of them) keep state specific to the current process.

There are two kinds of process-specific variables: process-local (state available
for read and write in all methods inside the process), and dynamic variables
(implementing dynamic scope).

My subclasses could supply inheritable values which will be installed on forked (child) processes. To enable this classes should return true from  #isInheritable method (on class side).
Also subclasses could provide specific logic for installing variables into new processes which allows interesting hooks to control forked processes
