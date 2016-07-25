I represent an unpacking object that is able to receive serialized objects, e.g., FUEL or STON, and tries all available packing systems to unpack the serialized objects.

Others can ask me for materializing an object by sending me #unpack:. I resent the request to GTEventPacking objects and if anyone is able to materialize it, I will return the materialized object. Otherwise, I raise an error.

Collaborators Part: GTEventPacking, GTEventInitializationError, GTEventUnpackingError.

Public API and Key Messages

- unpack: aByteArray
- addPacking: aGTEventPacking
- how to create instances: #default on the class side.

Example:
	(GTEventUnpacking default
		unpack: (GTEventPacking ston 
					pack: (GTEventCollector new 
							add: 1; add: 2; add: 3; yourself)) 
			data) unpackedData
 
Internal Representation and Key Implementation Points.

    Instance Variables
	packings:		<OrderedCollection>
