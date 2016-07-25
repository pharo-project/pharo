accessing
selected
	"Answer the value of selected"
	selected ifNil: [ selected := self firstVisible ].
	^ selected