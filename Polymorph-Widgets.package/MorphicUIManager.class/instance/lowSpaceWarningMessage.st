debug
lowSpaceWarningMessage
	"Return a notifier message string to be presented when space is running low."

	^ 'Warning! Pharo is almost out of memory!

Low space detection is now disabled. It will be restored when you close or proceed from this error notifier. Don''t panic, but do proceed with caution.

Here are some suggestions:

 If you suspect an infinite recursion (the same methods calling each other again and again), then close this debugger, and fix the problem.

 If you want this computation to finish, then make more space available (read on) and choose "proceed" in this debugger. Here are some ways to make more space available...
   > Close any windows that are not needed.
   > Get rid of some large objects (e.g., images).
   > Leave this window on the screen, choose "save as..." from the screen menu, quit, restart the Pharo VM with a larger memory allocation, then restart the image you just saved, and choose "proceed" in this window.

 If you want to investigate further, choose "debug" in this window.  Do not use the debugger "fullStack" command unless you are certain that the stack is not very deep. (Trying to show the full stack will definitely use up all remaining memory if the low-space problem is caused by an infinite recursion!).

'
