utilities api
closeAllDebuggers
	"GTGenericStackDebugger closeAllDebuggers"
	(SystemWindow allSubInstances select: [:w | 
		 w model isKindOf: GTGenericStackDebugger])
			do: [:w | w delete ].
	GTSpecPreDebugWindow allInstances do: #delete.