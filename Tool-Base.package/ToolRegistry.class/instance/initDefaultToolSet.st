initialize-release
initDefaultToolSet
	
	"Query the system for registering tools"
	"we cannot afford to show warnings because not all tools e.g Debugger, Inspector are registered yet"
	
	[	
	Object withAllSubclassesDo: [:ea | 
		
	"evaluate only for classes which directly implement this message,
	not via subclassing"
		(ea isMeta  and: [
			ea includesSelector: #registerToolsOn: ]) ifTrue: [
			 ea theNonMetaClass registerToolsOn: self  ]].
	] on: Warning do: [:ex | ex resume ]