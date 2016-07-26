private
requestDropStream: dropIndex
	"Needs to install proper converter"
	
	| result |
	result := super requestDropStream: dropIndex.
	result ifNotNil: [
		converter ifNil: [self converter: UTF8TextConverter new].
		lineEndConvention ifNil: [ self detectLineEndConvention] ].
	^result