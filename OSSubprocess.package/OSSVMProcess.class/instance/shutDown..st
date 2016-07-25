system startup & shutdown
shutDown: quitting
	"If we have a process that do not stop automatically (like a tail -f) and it runs
	with #fork  (at Smalltalk level)	then that process will be continue running 
	after the image has stopped and started again. 
	If this process continues running, it may be waiting via a delay polling
	and that will throw an error since waitpid() will fail since the parent process 
	(the VM process) is NOT the parent anymore of the child process.
	And so.if the process fails, it makes the system startup to fail and we cannot
	open the image anymore. See https://github.com/marianopeck/OSSubprocess/issues/12
	So... to solve this we simply make all active childs to stop waiting so that the 
	next startup of the image has no problem. 
	
	"
	quitting ifTrue: [ self activeChildren do: [ :each | each shutDown: quitting ] ].