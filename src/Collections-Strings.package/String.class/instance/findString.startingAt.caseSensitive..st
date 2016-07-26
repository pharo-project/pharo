accessing
findString: key startingAt: start caseSensitive: caseSensitive 
	"Answer the index in this String at which the substring key first occurs,
	at or beyond start. The match can be case-sensitive or not. If no match
	is found, zero will be returned."
	
	"IMPLEMENTATION NOTE: do not use CaseSensitiveOrder because it is broken for WideString
	This is a temporary work around until Wide CaseSensitiveOrder search is fixed
	Code should revert to:
	caseSensitive
		ifTrue: [^ self findSubstring: key in: self startingAt: start matchTable: CaseSensitiveOrder]
		ifFalse: [^ self findSubstring: key in: self startingAt: start matchTable: CaseInsensitiveOrder]"
		
	^caseSensitive
		ifTrue: [
			(self class isBytes and: [key class isBytes])
				ifTrue: [self
						findSubstring: key
						in: self
						startingAt: start
						matchTable: CaseSensitiveOrder]
				ifFalse: [WideString new
						findSubstring: key
						in: self
						startingAt: start
						matchTable: nil]]
		ifFalse: [
			(self class isBytes and: [key class isBytes])
				ifTrue: [self
						findSubstring: key
						in: self
						startingAt: start
						matchTable: CaseInsensitiveOrder]
				ifFalse: [WideString new
						findSubstring: key
						in: self
						startingAt: start
						matchTable: CaseInsensitiveOrder]]