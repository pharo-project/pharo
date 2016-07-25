protocol
acceptClassSide: source notifying: aController
	| isClassDefinition |
	
	isClassDefinition := (' ' split: source) first = self model selectedClass theNonMetaClass name.
	
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