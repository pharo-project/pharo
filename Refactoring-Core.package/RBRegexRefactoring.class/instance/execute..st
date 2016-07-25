private
execute: aString
	"Perform all searches on aString and return the transformation."
	
	^ matchers 
		inject: aString asString
		into: [ :string :assoc | self execute: string replace: assoc key with: assoc value ]