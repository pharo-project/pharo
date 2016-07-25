*Morphic-Base
installNewWorld
	"let us play God."
	
	 [  |  man |
      
        man := MorphicUIManager new.
        "we do not use UIManager default because in headless mode there is not such process."
        man uiProcess ifNotNil: [ man terminateUIProcess  ].
       self new installForUIProcessReinstall.
        man spawnNewProcess.
     
    ] forkAt:  Processor userInterruptPriority