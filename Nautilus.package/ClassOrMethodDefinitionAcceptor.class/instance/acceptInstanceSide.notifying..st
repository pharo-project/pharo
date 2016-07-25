protocol
acceptInstanceSide: source notifying: aController
	| isClassDefinition |
	
	isClassDefinition := false.
	
	"Try to parse the source. If it succeed, it means the source represents a method definition. On error, we switch to class definition mode"
	[ RBParser parseMethod: source ]
		on: Notification
		do: [ isClassDefinition := true ]. 

	isClassDefinition
		ifTrue: [
			self model
				compileAClassFrom: source 
				notifying: aController ]
		ifFalse: [
			self model
				compileAMethodFromCategory: Protocol unclassified
				withSource: source
				notifying: aController ]