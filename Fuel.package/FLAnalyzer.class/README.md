I am responsible of traversing the graph of references starting from a root object. I will produce a clusterization which the serializer will store on a stream.

An example of use is:

	(FLAnalyzer newDefault 
		clusterizationOf: (Array with: 1@2 with: 3@4))
		clusters.
