printing
standardMessageText
	^ String streamContents: [ :s |
		s << ' '.
		self printObject: object on: s. 
		s << ' is read-only, hence its field '.
		fieldIndex printOn: s.
		s << ' cannot be modified with '.
		self printObject: newValue on: s]