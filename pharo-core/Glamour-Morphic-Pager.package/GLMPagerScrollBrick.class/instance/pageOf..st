accessing
pageOf: aSubMorph
	"returns 0 if morph is not in pager"
	|parent page|	
	
	aSubMorph ifNil: [ ^ 0 ].
	
	parent := aSubMorph.
	
	[parent isNotNil & ((parent == self band) not) ] whileTrue: [
		page := parent.
		parent := parent owner.
	].
	
	^ parent isNil ifTrue: [ 0 ] ifFalse: [ self panes indexOf: page. ].
	