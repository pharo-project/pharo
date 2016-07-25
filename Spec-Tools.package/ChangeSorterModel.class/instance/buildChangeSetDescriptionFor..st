text
buildChangeSetDescriptionFor: changeSet
		
	^ changeSet
		ifNil: [ '' ] 
		ifNotNil: [ changeSet preambleString ifNil: ['']]