process state change
yield
	"Give other Processes at the current priority a chance to run."

	| semaphore |

	<primitive: 167>
	semaphore := Semaphore new.
	[semaphore signal] fork.
	semaphore wait