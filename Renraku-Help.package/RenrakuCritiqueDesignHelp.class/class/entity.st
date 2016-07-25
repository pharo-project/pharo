pages
entity

	^ HelpTopic
		title: 'Entity Reference'
		contents: 
'A critique has a reference to the criticized entity.

This link is established through ReSourceAnchor. A source anchor has a reference to the actual class, method, or other entity that is criticized. An anchor also has a #providesInterval method that returns a boolean indicating if the anchor provides a selection interval to the actual source of the critique. The interval can be accessed through the #interval method.

There are two subclasses of ReSourceAnchor.
ReIntervalSourceAnchor stores the actual interval object which is set during initialization.
ReSearchStringSourceAnchor stores a searchString which will be searched for in the entities source code on demand to find an interval of substring'