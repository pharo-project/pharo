as yet unclassified
normalizePaneIndex: anIndex
	"retunes normalized pane index. For example: returns 1 if index < 1 and size if index > self size"
	
	anIndex < 1 ifTrue: [ ^ 1 ].
	anIndex > self size ifTrue: [ ^ self size ].
	^ anIndex.