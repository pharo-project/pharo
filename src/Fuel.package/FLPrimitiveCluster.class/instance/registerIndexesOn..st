serialize/materialize
registerIndexesOn: anEncoder

	self flag: #todo. "Converting objects collection here in this method is a bit confusing. This is because since this cluster is for primitives, they do not have pointers to other objects. Hence, instead of storing them in a IdentitySet we can use an OrderedCollection and then just convert them at the end. For more details see FLPrimitiveCluster >> #add: anObject traceWith: aAnalysis"
	objects := objects asIdentitySet.
	super registerIndexesOn: anEncoder