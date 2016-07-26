private
sessionIdFromRequest: request
	| cookie |
	cookie := request cookies 
		detect: [ :each | each name = self cookieName ] 
		ifNone: [ ^ nil ].
	^ cookie value