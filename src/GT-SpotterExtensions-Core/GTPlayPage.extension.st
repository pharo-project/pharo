Extension { #name : #GTPlayPage }

{ #category : #'*GT-SpotterExtensions-Core' }
GTPlayPage >> spotterActDefault [
	| result |
	result := GTPlayground new openOn: self.
	"Spotter only sets the title without installing the stash persistency.
	This is because when openinng the Playground the changing of the text triggers a save and this implies resaving the file under a new name and this is not wanted for the named pages. 
	So, we work around that and we ensure that the stash is created after the playground is created.
	This code has to be revisited."
	self title notNil ifTrue: [ self ensureStashPersistency ].
	^ result
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTPlayPage >> spotterPreviewCodeIn: aComposite [
	<spotterPreview: 10>
	aComposite pharoMethod
		title: 'Preview';
		display: #content;
		smalltalkClass: [ nil ];
		entity: self.
		
	self flag: 'move "entity: self" somewhere else, maybe'
]
