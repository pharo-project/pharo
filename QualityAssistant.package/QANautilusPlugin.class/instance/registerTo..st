registration
registerTo: aModel

	aModel announcer
		when: NautilusPackageSelected  send: #packageSelected:  to: self;
 		when: NautilusClassSelected    send: #classSelected:  to: self; 
		when: NautilusProtocolSelected send: #otherSelected:  to: self; 	 
		when: NautilusMethodSelected   send: #methodSelected: to: self.
		
	ReSystemAnnouncer uniqueInstance weak
		when: ReCritiqueBanned
		send: #critiqueBanned:
		to: self
	