printing
standardMessageText
	^ String streamContents: [ :s |
		s << ' '.
		self printObject: object on: s. 
		s << ' is read-only, hence its field '.
		index printOn: s.
		s << ' cannot be modified with '.
		self printObject: value on: s. 
		s << ' through '.
		selector printOn: s ]