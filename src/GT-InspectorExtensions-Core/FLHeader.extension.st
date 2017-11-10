Extension { #name : #FLHeader }

{ #category : #'*GT-InspectorExtensions-Core' }
FLHeader >> gtGetMetaData [
	|assoc|
	
	assoc := OrderedCollection new.
	assoc add: ('Meta' ->(String streamContents: [ :stream |
		additionalObjects do: [ :add | add printOn: stream. stream crlf ]])).
	assoc add: ('Pre materialization actions:' -> (String streamContents: [ :stream |
		preMaterializationActions do: [ :action | action printOn: stream ]])).
	assoc add: ('Post materialization actions: ' -> (String streamContents: [ :stream |
		postMaterializationActions do: [ :action | action printOn: stream ]])).
	^ assoc
]
