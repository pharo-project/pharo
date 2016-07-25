I am using semaphore to signal/wait for new items available in queue

I am introducing extra protocol - #next,
which blocks the sender until it can successfully fetch next item from queue.
