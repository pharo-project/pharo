A Mutex is a light-weight MUTual EXclusion object being used when two or more processes need to access a shared resource concurrently. A Mutex grants ownership to a single process and will suspend any other process trying to aquire the mutex while in use. Waiting processes are granted access to the mutex in the order the access was requested.

Instance variables:
	semaphore	<Semaphore>		The (primitive) semaphore used for synchronization.
	owner		<Process>		The process owning the mutex.