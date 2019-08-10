Extension { #name : #PluggableListMorph }

{ #category : #'*Deprecated80' }
PluggableListMorph >> doubleClick: event onItem: aMorph [
	self deprecated: 'This feature is not working and will be removed.' on: '07-28-2019' in: #Pharo8.
	self removeObsoleteEventHandlers.
]

{ #category : #'*Deprecated80' }
PluggableListMorph >> mouseDown: event onItem: aMorph [
	self deprecated: 'This feature is not working and will be removed.' on: '07-28-2019' in: #Pharo8.
	self removeObsoleteEventHandlers.
]

{ #category : #'*Deprecated80' }
PluggableListMorph >> mouseEnterDragging: anEvent onItem: aMorph [ 
	self deprecated: 'This feature is not working and will be removed.' on: '07-28-2019' in: #Pharo8.
	self removeObsoleteEventHandlers.
]

{ #category : #'*Deprecated80' }
PluggableListMorph >> mouseLeaveDragging: anEvent onItem: aMorph [ 
	self deprecated: 'This feature is not working and will be removed.' on: '07-28-2019' in: #Pharo8.
	self removeObsoleteEventHandlers.
]

{ #category : #'*Deprecated80' }
PluggableListMorph >> mouseUp: event onItem: aMorph [ 
	self deprecated: 'This feature is not working and will be removed.' on: '07-28-2019' in: #Pharo8.
	self removeObsoleteEventHandlers.
]

{ #category : #'*Deprecated80' }
PluggableListMorph >> removeObsoleteEventHandlers [
	self deprecated: 'This feature is not working and will be removed.' on: '07-28-2019' in: #Pharo8.
	scroller submorphs do:[:m|
		m eventHandler: nil; highlightForMouseDown: false; resetExtension].
]

{ #category : #'*Deprecated80' }
PluggableListMorph >> startDrag: evt onItem: itemMorph [ 
	self deprecated: 'This feature is not working and will be removed.' on: '07-28-2019' in: #Pharo8.
	self removeObsoleteEventHandlers.
]
