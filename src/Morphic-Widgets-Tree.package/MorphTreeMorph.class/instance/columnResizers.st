column handling
columnResizers
	"return the resizers which make it possible 
	to resize columns horizontally with the mouse"
	^ columnResizers ifNil: [ columnResizers := OrderedCollection new ]