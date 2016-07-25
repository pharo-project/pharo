checking
raiseWarningsIfAny: warnings

	warnings ifNotEmpty: [
		Warning new
			messageText: 'VeryDeepCopy out of sync in some classes. Some classes contain veryDeepCopyWith: or veryDeepInner: methods that are not in sync with their instance variables. Check the exception #tag for a list of them';
			tag: warnings;
			signal ]