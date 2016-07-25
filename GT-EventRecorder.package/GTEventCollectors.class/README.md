I represent a collection of GTEventCollector objects. GTEventRecorder uses me and I am responsible for adding and removing the collectors.

Public API and Key Messages

- add:  
- remove:
 
Internal Representation and Key Implementation Points.

    Instance Variables
	collectors:		<OrderedCollection>
	mutex:		<Mutex>
