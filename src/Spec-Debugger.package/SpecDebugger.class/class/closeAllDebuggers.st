utilities
closeAllDebuggers
	"SpecDebugger closeAllDebuggers"
	(MorphicWindowAdapter allInstances select: [:w | 
		(w model isKindOf: WindowPresenter) and: [ 
			w model model isKindOf: self ]  ])
			do: [:w | w delete ].
			
	SpecPreDebugWindow allInstances do: #delete.
			
		
			
	