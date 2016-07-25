testing
isResponsibleFor: aUrl
	^ (aUrl includesSubstring: 'ss3.gemstone.com/') or: [  aUrl includesSubstring: 'ss3.gemtalksystems.com/' ]