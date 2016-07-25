private
privateReplaceFrom: start to: stop with: aText

	super privateReplaceFrom: start to: stop with: (self sanitize: aText)