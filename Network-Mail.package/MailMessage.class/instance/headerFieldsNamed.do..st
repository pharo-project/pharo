parsing
headerFieldsNamed: fieldName do: aBlock 
	"Evalue aBlock once for each header field which matches fieldName.  The block is valued with one parameter, the value of the field"
	self 
		fieldsFrom: text readStream
		do: [ :fName :fValue | (fieldName sameAs: fName) ifTrue: [ aBlock value: fValue ] ]