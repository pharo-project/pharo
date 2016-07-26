OSSVMProcess represents the operating system process in which this Pharo VM is currently running. OSSVMProcess has a unique instance accessed via #vmProcess and it also uses a unique instance of  OSSUnixSystemAccessor  which provides access to the external operating system.

OSSVMProcess can answer some information about the OS process running the VM, such as running PID, children, etc etc. More can be added later. 

Another important task of this class is to keep track of all the launched children processes (instances of OSSUnixSubprocess). Whenever a process is started it's registered in OSSVMProcess and unregister in certain scenarios (see senders of #unregisterChildProcess:). We keep a  list of all our children, and ocasionally prune all those that have already been exited. 

This class takes care of running what we call the "child watcher" which is basically a way to monitor children status and collect exit code when they finish. This also  guarantees not to let zombies process (a child whose parent did not collected child exit status). Basically, we use a SIGCHLD handler to capture  a child death. For more details, see method #initializeChildWatcher.
 