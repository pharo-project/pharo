I am a VM Special class! Do not break me!

I am a linked list that contains processes as Nodes. My implementation is tied to the VM: 
- every node I contain should have as first instance variable the next node in the list.

My main user is ProcessScheduler, which contains an array with instances of myself. Each entry in that array a priority for processes. Processes are queues in each process list by the VM automatically.