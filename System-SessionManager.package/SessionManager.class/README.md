I'm the entry point of the session (betwen start/save/stop) management mechanism.
I replace the old startup and shutdown list mechanism.
There is a default instance of me used by the system accessible through:
	self default.
I can create new sessions and manage categories.
I also provide a facade to register new session handlers.

I can provide the list of handlers by order of priority (categories order and categories priority) either for the startup or the shutdown.
	self default startupList.
	self default shutdownList.
Note: the shutdown list is just the startup list reversed