childs management
updateActiveChildrenAndNotifyDead
	"Test each active child for its completion status and update exitStatus
	accordingly. This method may be called when a semaphore is set indicating that
	some child OSProcess has died (for example, from the child watcherer).
	It will also notify the dead child so that particular actions can be taken."

	self activeChildren do:
		[:child | child queryExitStatus ifNotNil: [ 
				"#activeChildren answers those whose exitStatus is not nil. 
				Tehrefore if now queryExitStatus answers notNil, then this is the child that die!!!"
				child processHasExitNotification.
			 ]
		 ]
