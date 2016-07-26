*GT-InspectorExtensions-Core
addAnnouncementFirst: ann

	^ self addFirst: (DateAndTime now -> ann)