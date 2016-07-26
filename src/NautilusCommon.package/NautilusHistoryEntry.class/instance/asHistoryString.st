protocol
asHistoryString
	| separator |
	separator := ' >>#'.
	^ String
		streamContents:
			[ :s | 
			self selectedPackage
				ifNotNil:
					[ :p | 
					self selectedClass
						ifNotNil:
							[ :c | 
							s << c name.
							self selectedSelector ifNotNil: [ :m | s << separator << m asString ] ] ] ]