system attribute
extractMinusParameters
	"Returns a dictionary matching parameters beginning by a minus character and the list of the next parameters.
	Here is an example: 
		I suppose I run pharo with: `pharo myImage.image 1 2 3 woah -minus1 'Hello' -minus2 -minus3 'Hello' 'World'`.
		This message will return a dictionary with: 
			* --> #(1 2 3 'woah')
			minus1 --> #('Hello')
			minus2 --> #()
			minus3 --> #('Hello' 'World')"
			
	| args str index |
	
	args := OrderedCollection new.

	str := self getSystemAttribute: (index := 2).
	[ str isNil ] whileFalse: [ args add: str. str := self getSystemAttribute: (index := index + 1) ].

	^ self extractMinusParametersFrom: args