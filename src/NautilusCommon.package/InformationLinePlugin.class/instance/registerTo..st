registration
registerTo: aModel

	aModel announcer
		when: NautilusPackageSelected send: #changed: to: self;
		when: NautilusClassSelected send: #changed: to: self;
		when: NautilusGroupSelected send: #changed: to: self;
		when: NautilusProtocolSelected send: #changed: to: self;
		when: NautilusMethodSelected send: #changed: to: self