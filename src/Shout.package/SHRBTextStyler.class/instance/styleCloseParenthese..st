private
styleCloseParenthese: aMessageNode
	aMessageNode parentheses
		ifNotEmpty: [ 
			aMessageNode parentheses
				reverseDo: [ :interval | 
					| pos |
					pos := interval last.
					parentheseLevel := parentheseLevel - 1.
					self addStyle: self currentClosedParenthesisStyleName from: pos to: pos ] ]