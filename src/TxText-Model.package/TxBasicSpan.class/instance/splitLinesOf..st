private
splitLinesOf: aString


	| lines |
	
	lines := OrderedCollection new.
	self string: aString linesDo: [:line | lines add: line ].

	^ lines