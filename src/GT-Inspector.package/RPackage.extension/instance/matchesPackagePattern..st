*GT-Inspector-Examples-Tools
matchesPackagePattern: aStringOrPattern
	^ self packageName = aStringOrPattern
		or: [ (aStringOrPattern match: self packageName) 
			or: [ [ self packageName matchesRegexIgnoringCase: aStringOrPattern ] 
						on: Error 
						do: [ false ] ] ]