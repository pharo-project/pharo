accessing
analyzer
	^ analyzer ifNil: [ analyzer := self defaultAnalyzer ]