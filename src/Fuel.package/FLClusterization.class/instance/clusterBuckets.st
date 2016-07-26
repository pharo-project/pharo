accessing
clusterBuckets
	"Answer a new collection with all the clusters that have been collected in trace. Note that order is imporant."

	^ OrderedCollection new 
		add: globalsBucket;
		add: primitivesBucket;
		add: metalevelClassSideBucket;
		add: metalevelInstanceSideBucket;
		add: baselevelBucket;
		add: postBaselevelBucket;
		add: substitutionsBucket;
		yourself.