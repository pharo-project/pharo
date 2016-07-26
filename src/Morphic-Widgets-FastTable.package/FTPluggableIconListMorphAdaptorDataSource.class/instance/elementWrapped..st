private
elementWrapped: object
	^ self wrapSelector 
		ifNotNil: [ :selector | self model perform: selector with: object ] 
		ifNil: [ object ]