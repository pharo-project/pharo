running
setUp
	super setUp.
	prototypes add: TextEmphasis bold;
		 add: TextEmphasis italic;
		 add: TextEmphasis narrow;
		 add: TextEmphasis normal;
		 add: TextEmphasis struckOut;
		 add: TextEmphasis underlined 