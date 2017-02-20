accessing
entities
	"returns the list of objects from the #entity port of each pane of the finder"
	^ self panes collect: [ :each | (each port: #entity) value ]