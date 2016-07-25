accessing
extractor

	^ extractor ifNil: [ extractor := self class defaultExtractor ]