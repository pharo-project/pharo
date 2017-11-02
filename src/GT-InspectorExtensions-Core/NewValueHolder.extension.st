Extension { #name : #NewValueHolder }

{ #category : #'*GT-InspectorExtensions-Core' }
NewValueHolder >> gtAnnouncer [
	^ self announcer
]

{ #category : #'*GT-InspectorExtensions-Core' }
NewValueHolder >> gtInspectorPresentationsIn: composite inContext: aGTInspector [
	composite 
		updateOn: Announcement from: self announcer.
	super gtInspectorPresentationsIn: composite inContext: aGTInspector
]
