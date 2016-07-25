accessing
stylerClass
	^ self class stylingClass ifNil: [NullTextStyler]