results
addProperties: cm
	"set the properties of cm according to properties saved"
	properties ifNotNil: [
		cm penultimateLiteral: properties.
		properties  method: cm.
		properties  pragmas do: [:each | each setMethod: cm] ].