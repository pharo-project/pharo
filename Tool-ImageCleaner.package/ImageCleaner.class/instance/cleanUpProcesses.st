cleaning
cleanUpProcesses

    (Process allInstances
               reject: [ :p |
                       {(InputEventFetcher default fetcherProcess).
                       (Processor activeProcess).
                       (WeakArray runningFinalizationProcess).
                      (Processor backgroundProcess).
                       (SmalltalkImage current lowSpaceWatcherProcess).
                       (UIManager default uiProcess).
                       (Delay schedulingProcess)} includes: p ])
               do: [ :p |
                       p suspend; terminate ]
