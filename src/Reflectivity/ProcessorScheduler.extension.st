Extension { #name : 'ProcessorScheduler' }

{ #category : '*Reflectivity' }
ProcessorScheduler >> allProcesses [

	^ Process allSubInstances
]

{ #category : '*Reflectivity' }
ProcessorScheduler >> findContextsForMethod: aMethod [
	"Take care: users should call within #valueUnpreemptively"

	^ self allProcesses flatCollect: [ :process |
		  process findContextsForMethod: aMethod ]
]
