accessing
rootClasses
	^ rootClasses ifNil: [ rootClasses := self privateRootClasses]