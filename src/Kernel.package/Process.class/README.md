I represent an independent path of control in the system. This path of control may be stopped (by sending the message suspend) in such a way that it can later be restarted (by sending the message resume). When any one of several paths of control can be advanced, the single instance of ProcessorScheduler named Processor determines which one will actually be advanced partly using the value of priority.

(If anyone ever makes a subclass of Process, be sure to use allSubInstances in anyProcessesAbove:.)


Process-specific storage: 

	An old implementation using #environmentAt: [ifAbsent:/put:] protocol are no longer supported.
	One must not use a process-specific storage (PSS) methods directly, and instead use ProcessSpecificVariable (or subclass) instances to access process-specific storage.
	
A new implemention is a revision towards making an access to PSS faster.

When new instance of ProcessSpecificVariable are created, it obtains an unique index, which is registered using #allocatePSKey: (see class side).
This allows to dynamically create as many process-specific variables as needed, and access them in fast manner via simple array index (instead of dictionary lookup,
as in previous implementation).

Another important aspect of new implementation is that all values in PSS are held weakly. This is done to prevent accidental memory leaks
as well as no need to manually unregistering a process-specific keys , once they are no longer in use.