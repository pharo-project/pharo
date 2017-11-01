This is a tickless scheduler. In case of no scheduled delay it will ask the VM to sleep indefinitely otherwise it sleep until the scheduled delay is due.

In contrast to the DelayMicrosecondScheduler the sleep duration will not be capped to one second.