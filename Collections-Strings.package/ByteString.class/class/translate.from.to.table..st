primitives
translate: aString from: start  to: stop  table: table
	"translate the characters in the string by the given table, in place"
	<primitive: 'primitiveTranslateStringWithTable' module: 'MiscPrimitivePlugin'>
	
	<var: #table  declareC: 'unsigned char *table'>
	<var: #aString  declareC: 'unsigned char *aString'>

	start to: stop do: [ :i |
		aString at: i put: (table at: (aString at: i) asciiValue+1) ]