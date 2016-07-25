api
displaySymbol
	"<api: #inspect>"
	"return the method selector used to get the label of the entity that is for entry completion"
	^ displaySymbol value ifNil: [ #printString ]