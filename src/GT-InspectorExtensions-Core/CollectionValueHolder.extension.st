Extension { #name : #CollectionValueHolder }

{ #category : #'*GT-InspectorExtensions-Core' }
CollectionValueHolder >> addAnnouncementFirst: ann [

	^ self addFirst: (DateAndTime now -> ann)
]
