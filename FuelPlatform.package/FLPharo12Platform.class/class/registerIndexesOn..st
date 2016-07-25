private-extensions-flprimitivecluster
registerIndexesOn: aDictionary
	<flExtensionOf: #FLPrimitiveCluster isSource: true>
	^ 'registerIndexesOn: aDictionary

		self flag: #todo. "Converting objects collection here in this method is a bit confusing. This is because since this cluster is for primitives, they do not have pointers to other objects. Hence, instead of storing them in a IdentitySet we can use an OrderedCollection and then just convert them at the end. For more details see FLPrimitiveCluster >> #add: anObject traceWith: aAnalysis"
		
		"this is a work around for sets that can not yet contain nil"
		objects := FLLargeIdentitySet new 
			addAll: objects; 
			yourself.
		super registerIndexesOn: aDictionary.'