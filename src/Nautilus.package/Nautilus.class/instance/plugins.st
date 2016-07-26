accessing
plugins

	^ plugins ifNil: [ plugins := OrderedCollection new ]