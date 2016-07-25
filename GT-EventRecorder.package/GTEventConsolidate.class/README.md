I consolidate GTEventCollector objects into one or more collectors according to their URL and category. That way we can pack together data, that belongs to the same application (= URL and category) and we can save them together on the server side as one bundle.

For the Collaborators Part: 
- GTEventCollector objects that I consolidate
- GTEventRecorder that uses me before packing process.

Public API and Key Messages

- collectors: set a collection of collectors that should be consolidated
- consolidated returns collection of GTEventCollector objects.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	collectors:		<OrderedCollection>
