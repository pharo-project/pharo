*GT-InspectorExtensions-Core
gtInspectorVariableNodesIn: aCollection
	"We override the variables with context specific ones"
	| tempVariableValueNodes tempIndex |
	
	tempIndex := 0. 
	tempVariableValueNodes := (self tempNames
						collect: [ :tempName ||value|
							tempIndex := tempIndex + 1.
							value := [self debuggerMap namedTempAt: tempIndex in: self] 
								on: Error 
								do: [value := (Text string: 'error obtaining field value' attribute: TextColor red) asString ].
							GTInspectorDynamicNode hostObject: self label: '[' , tempName , ']' value: value ]) 
								sort: [:aNode :anotherNode | aNode label < anotherNode label ].
	aCollection addAll: tempVariableValueNodes.
	super gtInspectorVariableNodesIn: aCollection