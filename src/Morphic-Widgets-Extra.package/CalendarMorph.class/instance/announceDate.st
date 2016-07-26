private
announceDate

	| announcement |
	announcement := ChoseDate of: self date from: self.
	announcer ifNotNil: [ announcer announce: announcement ].