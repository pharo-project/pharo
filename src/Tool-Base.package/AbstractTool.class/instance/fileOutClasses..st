class
fileOutClasses: aCollection
	"Print a description of the selected class onto a file whose name is the 
	category name followed by .st."

	self 
		do: [ :class | class theNonMetaClass fileOut ]
		on: aCollection
		displaying: 'Filing out classes...'.