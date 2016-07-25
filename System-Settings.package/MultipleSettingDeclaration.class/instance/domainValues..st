accessing
domainValues: aCollection 
	domainValues := aCollection asArray collect: [ :v | v settingFixedDomainValueNodeFrom: self]