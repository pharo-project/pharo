*Spec-Tools-VersionBrowser
text: aPairOfString

	(aPairOfString isText or: [aPairOfString isString])
		ifTrue: [
			self leftText: ''.
			self rightText: aPairOfString
			 ]
		ifFalse: [
			self leftText: aPairOfString first.
			self rightText: aPairOfString second ]