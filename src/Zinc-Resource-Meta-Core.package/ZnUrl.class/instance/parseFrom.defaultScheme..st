parsing
parseFrom: string defaultScheme: defaultScheme
	| start end index |
	start := 1.
	end := string size.
	(index := string indexOf: $#) > 0
		ifTrue: [ 
			self fragment: (self decodePercent: (string copyFrom: index + 1 to: end)).
			end := index - 1 ].
	(index := string indexOf: $?) > 0
		ifTrue: [
			self query: (ZnResourceMetaUtils parseQueryFrom: (ReadStream on: string from: index + 1 to: end)).
			end := index - 1 ].
	((index := string indexOfSubCollection: '://') > 0 and: [ index <= end ])
		ifTrue: [
			self scheme: (string copyFrom: 1 to: index - 1).
			start := index + 3 ]
		ifFalse: [ 
			((index := string indexOf: $:) > 0 
					and: [ index <= end 
						and: [ self isSchemeNotUsingDoubleSlash: (string copyFrom: 1 to: index - 1) ] ])
				ifTrue: [ 
					self scheme: (string copyFrom: 1 to: index - 1).
					start := index + 1 ]
				ifFalse: [ 
					defaultScheme ifNotNil: [ self scheme: defaultScheme ] ] ].
	self hasScheme 
		ifTrue: [  
			(index := string indexOf: $/ startingAt: start) > 0
				ifTrue: [
					self parseAuthority: string from: start to: index - 1.
					start := index ]
				ifFalse: [ 
					^ self parseAuthority: string from: start to: end ] ].
	self parsePath: (ReadStream on: string from: start to: end)