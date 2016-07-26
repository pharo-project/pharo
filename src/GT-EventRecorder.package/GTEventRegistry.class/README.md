I am weak holder of an object that uses GTEventCollector. If the object disappears from object memory (or #liberate message is sent), GTEventCollector can be removed from a GTEventRecorder object.

For the Collaborators Part: GTEventCollector

Public API and Key Messages

- object: 
- liberate
- isLiberated

Internal Representation and Key Implementation Points.

    Instance Variables
	weakHolder:		<Object>
