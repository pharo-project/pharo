accessing
addBlankIconsIfNecessary: anIcon 
	"If any of my items have an icon, ensure that all do by using 
	anIcon for those that don't"
	self items
		reject: [:each | each hasIconOrMarker]
		thenDo: [:each | each icon: anIcon]