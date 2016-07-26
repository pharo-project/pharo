initialization
cleanUp
 "Remove terminated processes from my suspended list"

       self suspendedProcesses keysDo: [:ea |
               ea isTerminated ifTrue: [self suspendedProcesses removeKey: ea]]