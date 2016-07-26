memory space
okayToProceedEvenIfSpaceIsLow
	"Return true if either there is enough memory to do so safely or if the user gives permission after being given fair warning."

	self garbageCollectMost > self lowSpaceThreshold ifTrue: [^ true].  "quick"
	self garbageCollect > self lowSpaceThreshold ifTrue: [^ true].  "work harder"

	^ self confirm:
'WARNING: There is not enough space to start the low space watcher.
If you proceed, you will not be warned again, and the system may
run out of memory and crash. If you do proceed, you can start the
low space notifier when more space becomes available simply by
opening and then closing a debugger (e.g., by hitting Cmd-period.)
Do you want to proceed?'
