process list
keyEventsDict

	^ keyEventsDict ifNil: [
		keyEventsDict := Dictionary newFromPairs:  #(
			$i #inspectProcess
			$I #exploreProcess
			$e #explorePointers
			$P #inspectPointers
			$t #terminateProcess
			$r #resumeProcess
			$s #suspendProcess
			$d #debugProcess
			$p #changePriority
			$m #messageTally
			$f #findContext
			$g #nextContext
			$a #toggleAutoUpdate
			$u #updateProcessList
			$S #signalSemaphore
			$k #moreStack			
			)
		]
